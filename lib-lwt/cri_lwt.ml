open Lwt.Infix
open Cri

let src = Logs.Src.create "cri-lwt"
module Log = (val Logs.src_log src : Logs.LOG)

let pp_error ppf = function
  | `Write err -> Mimic.pp_write_error ppf err
  | `Decoder err -> Decoder.pp_error_with_info ~pp:Protocol.pp_error ppf err  
  | `End_of_input -> Fmt.string ppf "End of input"
  | `Time_out -> Fmt.string ppf "time out"
  | #Encoder.error as err -> Encoder.pp_error ppf err
  | #Mimic.error as err -> Mimic.pp_error ppf err

let blit0 src src_off dst dst_off len =
  let src = Cstruct.to_bigarray src in
  Bigstringaf.blit src ~src_off dst ~dst_off ~len

let blit1 src src_off dst dst_off len =
  Bigstringaf.blit_to_bytes src ~src_off dst ~dst_off ~len

type reader_error =
  [ `End_of_input        
  | `Decoder of Protocol.error Decoder.info
  | `Time_out
  | Mimic.error ]

type writer_error = [ `Write of Mimic.write_error | Encoder.error ]

type error = [ reader_error | writer_error ]

type signal =
  [ Cstruct.t Mirage_flow.or_eof
  | `Continue | `Stop | `Time_out ]

let rec reader ?stop ~timeout ~push flow =
  let dec = Decoder.decoder () in
  let ke = Ke.Rke.create ~capacity:0x1000 Bigarray.char in
  let th, u = Lwt.wait () in
  Lwt.catch begin fun () ->
        Lwt_switch.add_hook stop (fun () -> Lwt.wakeup_later u `Stop ; Lwt.return_unit) ;
        Log.debug (fun m -> m "Launch the reader.") ;
        go ~stop:th ~timeout ~push dec ke flow (Protocol.decode dec Protocol.Any)
  end @@ fun exn ->
    Log.err (fun m -> m "Got an exception from the reader loop: %S." (Printexc.to_string exn)) ;
    push None ; Lwt.return_ok ()
and go ~stop ~timeout ~push dec ke flow = function
  | Decoder.Done (_committed, msg) ->
    ( try push (Some msg) with _ -> () ) ;
    Log.debug (fun m -> m "Pause and waiting message.") ;
    ( Lwt.pick [ (Lwt.pause () >|= fun () -> `Continue)
               ; stop ] >>= function
    | `Continue -> go ~stop ~timeout ~push dec ke flow (Protocol.decode dec Protocol.Any)
    | `Stop -> push None ; Lwt.return_ok () )
  | Decoder.Read { buffer; off; len; continue; } as state ->
    Log.debug (fun m -> m "Read the flow.") ;
    ( match Ke.Rke.N.peek ke with
    | [] ->
      ( Lwt.pick [ (Mimic.read flow >|= fun res -> (res :> (signal, _) result))
                 ; (stop >|= fun v -> Rresult.R.ok (v :> signal))
                 ; (timeout () >|= fun () -> Rresult.R.ok `Time_out) ] >>= function
      | Error err ->
        Log.err (fun m -> m "Got an error while reading on the flow: %a." Mimic.pp_error err) ;
        Lwt.return_error (err :> error)
      | Ok `Continue ->
        assert false (* XXX(dinosaure): it's safe! *)
      | Ok `Time_out ->
        Log.debug (fun m -> m "The client time-out on the reader.") ;
        push None ; Lwt.return_error `Time_out
      | Ok `Stop ->
        Log.debug (fun m -> m "The client wants to close the connection.") ;
        push None ; Lwt.return_ok ()
      | Ok `Eof ->
        Log.debug (fun m -> m "The connection was closed by peer.") ;
        push None ; Lwt.return_error `End_of_input
      | Ok (`Data cs) ->
        Log.debug (fun m -> m "<~ @[<hov>%a@]" (Hxd_string.pp Hxd.default) (Cstruct.to_string cs)) ;
        Ke.Rke.N.push ke ~blit:blit0 ~length:Cstruct.length ~off:0 ~len:(Cstruct.length cs) cs ;
        go ~stop ~timeout ~push dec ke flow state )
    | _ ->
      let len = min len (Ke.Rke.length ke) in
      Ke.Rke.N.keep_exn ke ~blit:blit1 ~length:Bytes.length ~off ~len buffer ;
      Ke.Rke.N.shift_exn ke len ;
      go ~stop ~timeout ~push dec ke flow (continue len) )
  | Decoder.Error err ->
    Log.err (fun m -> m "%a." pp_error (`Decoder err)) ;
    push None ; Lwt.return_error (`Decoder err)

let ( >>? ) = Lwt_result.bind

let rec writer ~timeout ~next flow =
  let enc = Encoder.encoder () in
  let tmp = Cstruct.create Decoder.io_buffer_size in
  let allocator len = Cstruct.sub tmp 0 len in
  Lwt.pause () >>= next >>= function
  | Some (prefix, send) ->
    go ~timeout ~next allocator enc flow (Protocol.encode ?prefix enc send)
  | None ->
    Log.debug (fun m -> m "End of the writer.") ;
    Lwt.return_ok ()
and go ~timeout ~next allocator enc flow = function
  | Encoder.Done ->
    Log.debug (fun m -> m "Pause and next operation to emit.") ;
    ( Lwt.pause () >>= next >>= function
    | Some (prefix, send) ->
      go ~timeout ~next allocator enc flow (Protocol.encode ?prefix enc send)
    | None ->
      Log.debug (fun m -> m "Terminate the writer.") ;
      Lwt.return_ok () )
  | Encoder.Write { buffer; off; len; continue; } ->
    let cs = Cstruct.of_string ~allocator ~off ~len buffer in
    Log.debug (fun m -> m "~> @[<hov>%a@]" (Hxd_string.pp Hxd.default) (Cstruct.to_string cs)) ;
    ( Lwt.pick [ (Mimic.write flow cs >>? fun () -> Lwt.return_ok `Done)
               ; (timeout () >>= fun () -> Lwt.return_ok `Time_out) ] >>= function
    | Ok `Time_out ->
      Log.err (fun m -> m "The client time-out on the writer") ;
      Lwt.return_error `Time_out
    | Ok `Done -> go ~timeout ~next allocator enc flow (continue len)
    | Error err ->
      Log.err (fun m -> m "Got an error while writing on the flow: %a." Mimic.pp_write_error err) ;
      Lwt.return_error (`Write err) )
  | Encoder.Error err -> Lwt.return_error (err :> error)

let ( >>? ) = Lwt_result.bind

type recv = unit -> (Cri.Protocol.prefix option * Cri.Protocol.message) option Lwt.t
type send = { send : 'a. ?prefix:Cri.Protocol.prefix -> 'a Cri.Protocol.t -> 'a -> unit } [@@unboxed]
type close = unit -> unit

let run ?stop ?timeout ctx =
  let timeout = match timeout with
    | Some timeout -> timeout
    | None -> let never, _ = Lwt.wait () in fun () -> never in
  let recv, push_recv = Lwt_stream.create () in
  let send, push_send = Lwt_stream.create () in
  let push_send v = try push_send v with _ -> () in
  `Fiber
    (Mimic.resolve ctx >>? fun flow ->
     Log.debug (fun m -> m "Connected to the IRC server.") ;
     Lwt.pick
       [ reader ?stop ~timeout ~push:(fun v -> try push_recv v with _ -> ()) flow
       ; writer ~timeout ~next:(fun () -> Lwt_stream.get send) flow ] >>= fun res ->
     Log.debug (fun m -> m "Reader or writer are resolved (with an error: %b)." (Rresult.R.is_error res)) ;
     (* XXX(dinosaure): with TLS, [Mimic.close] can time-out. *)
     Lwt.pick [ Mimic.close flow; timeout () ] >>= fun () -> match res with
     | Ok () -> Lwt.return_ok ()
     | Error err ->
       Log.err (fun m -> m "Got an error: %a." pp_error err) ;
       Lwt.return_error (err :> error)),
  (fun () -> Lwt_stream.get recv),
  { send= (fun ?prefix w v -> push_send (Some (prefix, Protocol.send w v))) },
  (fun () -> push_send None)
