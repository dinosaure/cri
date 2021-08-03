include Tls_mirage.Make (Socket)

type endpoint = Tls.Config.client * [ `host ] Domain_name.t option * Unix.sockaddr

let pp_sockaddr ppf = function
  | Unix.ADDR_UNIX v -> Fmt.pf ppf "<unix:%s>" v
  | Unix.ADDR_INET (v, p) -> Fmt.pf ppf "%s:%d" (Unix.string_of_inet_addr v) p

let connect (tls, host, sockaddr) =
  let open Lwt.Infix in
  Socket.connect sockaddr >>= function
  | Error `Closed -> Lwt.return_error `Closed
  | Ok flow ->
    client_of_flow tls ?host flow >>= fun flow ->
    Lwt.return flow
