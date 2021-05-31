let reporter ppf =
  let report src level ~over k msgf =
    let k _ =
      over () ;
      k () in
    let with_metadata header _tags k ppf fmt =
      Format.kfprintf k ppf
        ("%a[%a]: " ^^ fmt ^^ "\n%!")
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src) in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt in
  { Logs.report }

let ( <.> ) f g = fun x -> f (g x)

let () = Printexc.record_backtrace true
let () = Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ~utf_8:true ()
let () = Logs.set_reporter (reporter Fmt.stdout)
let () = Logs.set_level ~all:true (Some Logs.Debug)
let () = Random.self_init ()
let () = Mirage_crypto_rng_unix.initialize ()

let hostname = Domain_name.of_string_exn (Unix.gethostname ())

let user =
  { Cri.Protocol.username= "mirage.noisy.bot"
  ; Cri.Protocol.hostname
  ; Cri.Protocol.servername= Domain_name.of_string_exn "*"
  ; Cri.Protocol.realname= "A mirage noisy bot - https://github.com/dinosaure/cri" }

let noisy_bot = Cri.Nickname.of_string_exn "noisy-bot"
let mirage = Cri.Channel.of_string_exn "#mirage"
let ocaml = Cri.Channel.of_string_exn "#ocaml"

let host : [ `host ] Domain_name.t Mimic.value = Mimic.make ~name:"host"
let port : int Mimic.value = Mimic.make ~name:"port"
let inet_addr : Unix.inet_addr Mimic.value = Mimic.make ~name:"ipaddr"
let sockaddr, tcpip = Mimic.register ~name:"tcp/ip" (module Socket)
let cfg, _ = Mimic.register ~priority:10 ~name:"tls" (module Ttls)
let tls : Tls.Config.client Mimic.value = Mimic.make ~name:"cfg"

let gethostbyname host =
  match Unix.gethostbyname (Domain_name.to_string host) with
  | { Unix.h_addr_list; _ } when Array.length h_addr_list > 0 ->
    Lwt.return_some h_addr_list.(0)
  | _ -> Lwt.return_none
  | exception _ -> Lwt.return_none

let make_tcp inet_addr port =
  Lwt.return_some (Unix.ADDR_INET (inet_addr, port))

let make_tls tls host sockaddr = Lwt.return_some (tls, host, sockaddr)

let authenticator ~host:_ _ = Ok None
let default = Tls.Config.client ~authenticator ()

let ctx_of_uri uri =
  let ctx = Mimic.empty in
  match Uri.scheme uri with
  | Some "ircs" ->
    Mimic.add port (Option.value ~default:6697 (Uri.port uri)) ctx
    |> Mimic.fold sockaddr Mimic.Fun.[ req inet_addr; dft port 6697 ] ~k:make_tcp
    |> Mimic.fold inet_addr Mimic.Fun.[ req host ] ~k:gethostbyname
    |> Mimic.fold cfg Mimic.Fun.[ dft tls default; opt host; req sockaddr ] ~k:make_tls
    |> fun ctx ->
       let some v =
         try Mimic.add inet_addr (Unix.inet_addr_of_string v) ctx
         with _ -> Mimic.add host Domain_name.(host_exn (of_string_exn v)) ctx in
       Option.fold ~none:ctx ~some (Uri.host uri)
  | Some "irc" | None ->
    Mimic.add port (Option.value ~default:6667 (Uri.port uri)) ctx
    |> Mimic.fold sockaddr Mimic.Fun.[ req inet_addr; dft port 6667 ] ~k:make_tcp
    |> Mimic.fold inet_addr Mimic.Fun.[ req host; ] ~k:gethostbyname
    |> fun ctx ->
       let some v =
         try Mimic.add inet_addr (Unix.inet_addr_of_string v) ctx
         with _ -> Mimic.add host Domain_name.(host_exn (of_string_exn v)) ctx in
       Option.fold ~none:ctx ~some (Uri.host uri)
  | _ -> Fmt.invalid_arg "Invalid uri: %a" Uri.pp uri

let sleep_ns = Lwt_unix.sleep <.> ( *. ) 1e-9 <.> Int64.to_float

let log _msgs = Lwt.return_unit

let run ctx =
  let open Lwt.Infix in
  let stop = Lwt_switch.create () in
  let state = Cri_logger.state ~user ~channel:ocaml ~nickname:noisy_bot
    ~tick:1_000_000_000L log in
  let `Fiber th, recv, send, close = Cri_lwt.run ~stop ~ctx in
  Lwt.both
    (th >>= function
     | Ok _ as res -> Lwt.return res
     | Error _ as res ->
       Lwt_switch.turn_off stop >|= close >>= fun () ->
       Lwt.return res)
    (Cri_logger.handler ~sleep_ns ~stop state recv send close) >>= function
  | Ok (), Ok () -> Lwt.return_unit
  | Error err, Ok () ->
    Fmt.epr "%a.\n%!" Cri_lwt.pp_error err ;
    Lwt.return_unit
  | Ok (), Error err ->
    Fmt.epr "%a.\n%!" Cri_logger.pp_error err ;
    Lwt.return_unit
  | Error err0, Error err1 ->
    Fmt.epr "%a.\n%!" Cri_lwt.pp_error err0 ;
    Fmt.epr "%a.\n%!" Cri_logger.pp_error err1 ;
    Lwt.return_unit

let () =
  let ctx = ctx_of_uri (Uri.of_string Sys.argv.(1)) in
  Lwt_main.run (run ctx)
