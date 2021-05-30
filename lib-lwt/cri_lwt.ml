open Lwt.Infix
open Cri

let src = Logs.Src.create "cri-lwt"
module Log = (val Logs.src_log src : Logs.LOG)

let pp_error ppf = function
  | `Write err -> Mimic.pp_write_error ppf err
  | `Decoder err -> Decoder.pp_error_with_info ~pp:Protocol.pp_error ppf err  
  | `End_of_input -> Fmt.string ppf "End of input"
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
  | Mimic.error ]

type writer_error = [ `Write of Mimic.write_error | Encoder.error ]

type error = [ reader_error | writer_error ]

type signal =
  [ Cstruct.t Mirage_flow.or_eof
  | `Continue | `Stop ]

let rec reader ?stop ~push flow =
  let dec = Decoder.decoder () in
  let ke = Ke.Rke.create ~capacity:0x1000 Bigarray.char in
  let th, u = Lwt.wait () in
  ( try Lwt_switch.add_hook stop (fun () -> Lwt.wakeup_later u `Stop ; Lwt.return_unit) ;
        Log.debug (fun m -> m "Launch the reader.") ;
        go ~stop:th ~push dec ke flow (Protocol.decode dec Protocol.Any)
    with _ -> push None ; Lwt.return_ok () )
and go ~stop ~push dec ke flow = function
  | Decoder.Done msg ->
    ( try push (Some msg) with _ -> () ) ;
    Log.debug (fun m -> m "Pause and waiting message.") ;
    ( Lwt.pick [ (Lwt.pause () >|= fun () -> `Continue)
               ; stop ] >>= function
    | `Continue -> go ~stop ~push dec ke flow (Protocol.decode dec Protocol.Any)
    | `Stop -> push None ; Lwt.return_ok () )
  | Decoder.Read { buffer; off; len; continue; } as state ->
    ( match Ke.Rke.N.peek ke with
    | [] ->
      ( Lwt.pick [ (Mimic.read flow >|= fun res -> (res :> (signal, _) result))
                 ; (stop >|= fun v -> Rresult.R.ok (v :> signal)) ] >>= function
      | Error err -> Lwt.return_error (err :> error)
      | Ok `Continue -> assert false (* XXX(dinosaure): it's safe! *)
      | Ok `Stop ->
        push None ; Lwt.return_ok ()
      | Ok `Eof ->
        push None ; Lwt.return_error `End_of_input
      | Ok (`Data cs) ->
        Ke.Rke.N.push ke ~blit:blit0 ~length:Cstruct.length ~off:0 ~len:(Cstruct.length cs) cs ;
        go ~stop ~push dec ke flow state )
    | _ ->
      let len = min len (Ke.Rke.length ke) in
      Ke.Rke.N.keep_exn ke ~blit:blit1 ~length:Bytes.length ~off ~len buffer ;
      Ke.Rke.N.shift_exn ke len ;
      go ~stop ~push dec ke flow (continue len) )
  | Decoder.Error err ->
    Log.err (fun m -> m "%a." pp_error (`Decoder err)) ;
    push None ; Lwt.return_error (`Decoder err)

let rec writer ~next flow =
  let enc = Encoder.encoder () in
  let tmp = Cstruct.create Decoder.io_buffer_size in
  let allocator len = Cstruct.sub tmp 0 len in
  Lwt.pause () >>= next >>= function
  | Some (prefix, send) ->
    go ~next allocator enc flow (Protocol.encode ?prefix enc send)
  | None ->
    Log.debug (fun m -> m "End of the writer.") ;
    Lwt.return_ok ()
and go ~next allocator enc flow = function
  | Encoder.Done ->
    Log.debug (fun m -> m "Pause and next operation to emit.") ;
    ( Lwt.pause () >>= next >>= function
    | Some (prefix, send) ->
      go ~next allocator enc flow (Protocol.encode ?prefix enc send)
    | None ->
      Log.debug (fun m -> m "Terminate the writer.") ;
      Lwt.return_ok () )
  | Encoder.Write { buffer; off; len; continue; } ->
    let cs = Cstruct.of_string ~allocator ~off ~len buffer in
    ( Mimic.write flow cs >>= function
    | Ok () -> go ~next allocator enc flow (continue len)
    | Error err -> Lwt.return_error (`Write err) )
  | Encoder.Error err -> Lwt.return_error (err :> error)

let ( >>? ) = Lwt_result.bind

type recv = unit -> (Cri.Protocol.prefix option * Cri.Protocol.message) option Lwt.t
type send = { send : 'a. ?prefix:Cri.Protocol.prefix -> 'a Cri.Protocol.t -> 'a -> unit } [@@unboxed]
type close = unit -> unit

let run ?stop ~ctx =
  let recv, push_recv = Lwt_stream.create () in
  let send, push_send = Lwt_stream.create () in
  let push_send v = try push_send v with _ -> () in
  `Fiber
    (Mimic.resolve ctx >>? fun flow ->
     Lwt.pick
       [ reader ?stop ~push:(fun v -> try push_recv v with _ -> ())flow
       ; writer ~next:(fun () -> Lwt_stream.get send) flow ] >>= fun res ->
     Mimic.close flow >>= fun () -> match res with
     | Ok () -> Lwt.return_ok ()
     | Error err ->
       Lwt.return_error (err :> error)),
  (fun () -> Lwt_stream.get recv),
  { send= (fun ?prefix w v -> push_send (Some (prefix, Protocol.send w v))) },
  (fun () -> push_send None)
