open Rresult
open Lwt.Infix

let src = Logs.Src.create "cri.mirage"
module Log = (val Logs.src_log src : Logs.LOG)

let ipaddr : Ipaddr.t Mimic.value = Mimic.make ~name:"cri-ipaddr"
let domain_name : [ `host ] Domain_name.t Mimic.value = Mimic.make ~name:"cri-domain-name"
let port : int Mimic.value = Mimic.make ~name:"cri-port"
let cfg : Tls.Config.client Mimic.value = Mimic.make ~name:"cri-tls"
let scheme : string Mimic.value = Mimic.make ~name:"cri-scheme"

module Make
  (Random : Mirage_random.S)
  (Mclock : Mirage_clock.MCLOCK)
  (Pclock : Mirage_clock.PCLOCK)
  (Time : Mirage_time.S)
  (Stack : Tcpip.Stack.V4V6) = struct
  module TCP = struct
    include Stack.TCP

    type endpoint = Stack.t * Ipaddr.t * int

    type nonrec write_error =
      [ `Write of write_error | `Connect of error | `Closed ]

    let pp_write_error ppf = function
      | `Connect err -> pp_error ppf err
      | `Write err | (`Closed as err) -> pp_write_error ppf err

    let write flow cs =
      write flow cs >>= function
      | Ok _ as v -> Lwt.return v
      | Error err -> Lwt.return_error (`Write err)

    let writev flow css =
      writev flow css >>= function
      | Ok _ as v -> Lwt.return v
      | Error err -> Lwt.return_error (`Write err)

    let connect : endpoint -> _ = fun (stack, ipaddr, port) ->
      let stack = Stack.tcp stack in
      create_connection stack (ipaddr, port) >>= function
      | Ok _ as v -> Lwt.return v
      | Error err -> Lwt.return_error (`Connect err)
  end

  module TLS = struct
    let src = Logs.Src.create "cri.mirage.tls"
    module Log = (val Logs.src_log src : Logs.LOG)


    include Tls_mirage.Make (TCP)

    type endpoint = Tls.Config.client * [ `host ] Domain_name.t option * Stack.t * Ipaddr.t * int

    let connect (cfg, host, stack, ipaddr, port) =
      Log.debug (fun m -> m "Try to initiate a TLS connection to %a:%d (%a)."
        Ipaddr.pp ipaddr port Fmt.(option Domain_name.pp) host) ;
      TCP.connect (stack, ipaddr, port) >>= function
      | Ok flow -> client_of_flow cfg ?host flow
      | Error err -> Lwt.return_error (`Write err)
  end
      
  let tcp_edn, _tcp_protocol = Mimic.register ~name:"cri-tcpip" (module TCP)
  let tls_edn, _tcp_protocol = Mimic.register ~priority:10 ~name:"cri-tls" (module TLS)

  let stack : Stack.t Mimic.value = Mimic.make ~name:"cri-stack"
  let with_stack v ctx = Mimic.add stack v ctx

  module DNS = Dns_client_mirage.Make (Random) (Time) (Mclock) (Pclock) (Stack)

  let dns : DNS.t Mimic.value = Mimic.make ~name:"cri-dns"
  let with_dns ?cache_size ?nameservers ?timeout v ctx =
    let v = DNS.create ?cache_size ?nameservers ?timeout v in
    Mimic.add dns v ctx

  let authenticator ?ip:_ ~host:_ _ = Ok None

  let ctx =
    let k0 scheme stack ipaddr port = match scheme with
      | "irc" -> Lwt.return_some (stack, ipaddr, port)
      | _ -> Lwt.return_none in
    let k1 cfg domain_name stack ipaddr port =
      Lwt.return_some (cfg, domain_name, stack, ipaddr, port) in
    let k2 dns domain_name =
      Log.debug (fun m -> m "Try to resolve %a." Domain_name.pp domain_name) ;
      DNS.gethostbyname dns domain_name >>= function
      | Ok ipv4 ->
        Log.debug (fun m -> m "DNS: %a -> %a." Domain_name.pp domain_name Ipaddr.V4.pp ipv4) ;
        Lwt.return_some (Ipaddr.V4 ipv4)
      | _ ->
        Log.warn (fun m -> m "No IPv4 found for %a." Domain_name.pp domain_name) ;
        Lwt.return_none in
    Mimic.empty
    |> Mimic.fold tcp_edn Mimic.Fun.[ req scheme; req stack; req ipaddr; dft port 6665 ] ~k:k0
    |> Mimic.fold tls_edn Mimic.Fun.[ dft cfg (Tls.Config.client ~authenticator ())
                                    ; opt domain_name
                                    ; req stack
                                    ; req ipaddr
                                    ; dft port 6697 ] ~k:k1
    |> Mimic.fold ipaddr Mimic.Fun.[ req dns; req domain_name ] ~k:k2
end

let ( <.> ) f g = fun x -> f (g x)

let ctx_of_uri uri =
  let ctx = Mimic.empty in
  let ctx = Option.fold ~none:ctx ~some:(fun v -> Mimic.add scheme v ctx) (Uri.scheme uri) in
  let ctx = match Uri.host uri with
    | None -> ctx
    | Some host -> match R.(Domain_name.of_string host >>= Domain_name.host), Ipaddr.of_string host with
      | _, Ok v -> Mimic.add ipaddr v ctx
      | Ok v, _ -> Mimic.add domain_name v ctx
      | _ -> ctx in
  let ctx = Option.fold ~none:ctx ~some:(fun v -> Mimic.add port v ctx) (Uri.port uri) in
  ctx
