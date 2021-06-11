type endpoint = Unix.sockaddr

type error = Unix.error * string * string
type write_error = [ `Closed | `Unix of (Unix.error * string * string) ]

let pp_error : error Fmt.t = fun ppf (err, f, args) ->
  Fmt.pf ppf "%s(%s): %s" f args (Unix.error_message err)

let pp_write_error : write_error Fmt.t = fun ppf -> function
  | `Closed -> Fmt.string ppf "Connection closed by peer"
  | `Unix (err, f, args) -> Fmt.pf ppf "%s(%s): %s" f args (Unix.error_message err)

type flow = Lwt_unix.file_descr

open Lwt.Infix

let connect sockaddr =
  let domain = Unix.domain_of_sockaddr sockaddr in
  let socket = Lwt_unix.socket domain Unix.SOCK_STREAM 0 in
  Lwt_unix.connect socket sockaddr >>= fun () ->
  Lwt_unix.set_blocking socket false ; (* See RFC 1459, 8.11 *)
  Lwt.return_ok socket

let protect f =
  Lwt.catch f @@ function
  | Unix.Unix_error (err, f, args) -> Lwt.return_error (err, f, args)
  | exn -> raise exn

let read flow =
  let tmp = Bytes.create 0x1000 in
  let process () =
    Lwt_unix.read flow tmp 0 0x1000 >>= function
    | 0 -> Lwt.return_ok `Eof
    | len -> Lwt.return_ok (`Data (Cstruct.of_bytes ~off:0 ~len tmp)) in
  protect process

let rec write flow cs =
  go flow (Cstruct.to_bytes cs) 0 (Cstruct.length cs)
and go flow tmp off len =
  Lwt.catch begin fun () ->
    Lwt_unix.write flow tmp off len >>= fun len' ->
    if len - len' = 0 then Lwt.return_ok ()
    else go flow tmp (off + len') (len - len')
  end @@ function
  | Unix.Unix_error (err, f, args) -> Lwt.return_error (`Unix (err, f, args))
  | exn -> raise exn

let ( >>? ) = Lwt_result.bind

let rec writev flow css = go flow css
and go flow = function
  | [] -> Lwt.return_ok ()
  | cs :: css -> write flow cs >>? fun () -> go flow css

let close flow = Lwt.catch (fun () -> Lwt_unix.close flow) (fun _exn -> Lwt.return_unit)
