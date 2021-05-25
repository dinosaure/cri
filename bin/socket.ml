type endpoint = Unix.sockaddr

type error = |
type write_error = [ `Closed ]

let pp_error : error Fmt.t = fun _ppf -> function _ -> .

let pp_write_error : write_error Fmt.t = fun ppf `Closed ->
  Fmt.string ppf "Connection closed by peer"

type flow = Lwt_unix.file_descr

open Lwt.Infix

let connect sockaddr =
  let domain = Unix.domain_of_sockaddr sockaddr in
  let socket = Lwt_unix.socket domain Unix.SOCK_STREAM 0 in
  Lwt_unix.connect socket sockaddr >>= fun () ->
  Lwt_unix.set_blocking socket false ; (* See RFC 1459, 8.11 *)
  Lwt.return_ok socket

let read flow =
  let tmp = Bytes.create 0x1000 in
  Lwt_unix.read flow tmp 0 0x1000 >>= function
  | 0 -> Lwt.return_ok `Eof
  | len -> Lwt.return_ok (`Data (Cstruct.of_bytes ~off:0 ~len tmp))

let rec write flow cs =
  go flow (Cstruct.to_bytes cs) 0 (Cstruct.length cs)
and go flow tmp off len =
  Lwt_unix.write flow tmp off len >>= fun len' ->
  if len - len' = 0 then Lwt.return_ok ()
  else go flow tmp (off + len') (len - len')

let ( >>? ) = Lwt_result.bind

let rec writev flow css = go flow css
and go flow = function
  | [] -> Lwt.return_ok ()
  | cs :: css -> write flow cs >>? fun () -> go flow css

let close flow = Lwt_unix.close flow
