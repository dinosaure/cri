open Lwt.Infix
open Cri

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

type signal =
  [ Cstruct.t Mirage_flow.or_eof
  | `Continue | `Stop ]

let rec reader ?stop ~push flow =
  let dec = Decoder.decoder () in
  let ke = Ke.Rke.create ~capacity:0x1000 Bigarray.char in
  let th, u = Lwt.wait () in
  Lwt_switch.add_hook stop (fun () -> Lwt.wakeup_later u `Stop ; Lwt.return_unit) ;
  go ~stop:th ~push dec ke flow (Protocol.decode dec Protocol.Any)
and go ~stop ~push dec ke flow = function
  | Decoder.Done msg ->
    push (Some msg) ;
    ( Lwt.pick [ (Lwt.pause () >|= fun () -> `Continue)
               ; stop ] >>= function
    | `Continue -> go ~stop ~push dec ke flow (Protocol.decode dec Protocol.Any)
    | `Stop -> push None ; Lwt.return_ok () )
  | Decoder.Read { buffer; off; len; continue; } as state ->
    ( match Ke.Rke.N.peek ke with
    | [] ->
      ( Lwt.pick [ (Mimic.read flow >|= fun res -> (res :> (signal, _) result))
                 ; (stop >|= fun v -> Rresult.R.ok (v :> signal)) ] >>= function
      | Error err -> Lwt.return_error (err :> reader_error)
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
    push None ; Lwt.return_error (`Decoder err)

type writer_error = [ `Write of Mimic.write_error | Encoder.error ]

let rec writer ~next flow =
  let enc = Encoder.encoder () in
  let tmp = Cstruct.create Decoder.io_buffer_size in
  let allocator len = Cstruct.sub tmp 0 len in
  next () >>= function
  | Some (prefix, send) ->
    go ~next allocator enc flow (Protocol.encode ?prefix enc send)
  | None -> Lwt.return_ok ()
and go ~next allocator enc flow = function
  | Encoder.Done ->
    ( next () >>= function
    | Some (prefix, send) ->
      go ~next allocator enc flow (Protocol.encode ?prefix enc send)
    | None -> Lwt.return_ok () )
  | Encoder.Write { buffer; off; len; continue; } ->
    let cs = Cstruct.of_string ~allocator ~off ~len buffer in
    ( Mimic.write flow cs >>= function
    | Ok () -> go ~next allocator enc flow (continue len)
    | Error err -> Lwt.return_error (`Write err) )
  | Encoder.Error err -> Lwt.return_error err

let ( >>? ) = Lwt_result.bind

type error = [ reader_error | writer_error ]

let run ?stop ~ctx =
  let recv, push_recv = Lwt_stream.create () in
  let send, push_send = Lwt_stream.create () in
  `Fiber
    (Mimic.resolve ctx >>? fun flow ->
     Lwt.both 
       (reader ?stop ~push:push_recv flow )
       (writer ~next:(fun () -> Lwt_stream.get send) flow) >>= fun res ->
     Mimic.close flow >>= fun () -> match res with
     | Ok (), Ok () -> Lwt.return_ok ()
     | Error err0, _ -> Lwt.return_error (err0 :> error)
     | _, Error err0 -> Lwt.return_error (err0 :> error)),
  (fun () -> Lwt_stream.get recv), push_send
