include Tls_mirage.Make (Socket)

type endpoint = Tls.Config.client * [ `host ] Domain_name.t option * Unix.sockaddr

let pp_sockaddr ppf = function
  | Unix.ADDR_UNIX v -> Fmt.pf ppf "<unix:%s>" v
  | Unix.ADDR_INET (v, p) -> Fmt.pf ppf "%s:%d" (Unix.string_of_inet_addr v) p

let connect (tls, domain_name, sockaddr) =
  let open Lwt.Infix in
  Socket.connect sockaddr >>= function
  | Error `Closed -> Lwt.return_error `Closed
  | Ok flow ->
    let host = Option.map Domain_name.to_string domain_name in
    Fmt.epr ">>> Initiate a TLS connection to %a.\n%!" pp_sockaddr sockaddr ;
    client_of_flow tls ?host flow >>= fun flow ->
    Fmt.epr ">>> Connected via TLS.\n%!" ;
    Lwt.return flow
