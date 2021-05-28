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

let () = Printexc.record_backtrace true
let () = Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ~utf_8:true ()
let () = Logs.set_reporter (reporter Fmt.stdout)
let () = Logs.set_level ~all:true (Some Logs.Debug)
let () = Random.self_init ()

let hostname = Domain_name.of_string_exn (Unix.gethostname ())

let romain_calascibetta =
  { Cri.Protocol.username= "mirage.noisy.bot"
  ; Cri.Protocol.hostname
  ; Cri.Protocol.servername= Domain_name.of_string_exn "*"
  ; Cri.Protocol.realname= "A mirage noisy bot - https://github.com/dinosaure/cri" }

let mirage =
  { Cri.Protocol.nick= Cri.Nickname.of_string_exn "noisy-bot"
  ; hopcount= None }

type state =
  | Connected
  | User_sended
  | Nick_sended
  | Join
  | Joined

let handler
  :    stop:Lwt_switch.t
    -> state ref
    -> Cri_lwt.recv
    -> Cri_lwt.send
    -> Cri_lwt.close
    -> unit Lwt.t
  = fun ~stop state recv { Cri_lwt.send } _close ->
  let open Lwt.Infix in
  let closed, u = Lwt.wait () in
  Lwt_switch.add_hook (Some stop) (fun () -> Lwt.wakeup_later u `Closed ; Lwt.return_unit) ;
  let rec writer () =
    Lwt.pick [ closed; (Lwt.pause () >|= fun () -> `Continue) ] >>= function
    | `Closed ->
      Logs.debug (fun m -> m "Quit") ;
      Lwt.return_unit
    | `Continue -> match !state with
      | Connected ->
        send Cri.Protocol.User romain_calascibetta ;
        state := User_sended ;
        writer ()
      | User_sended ->
        send Cri.Protocol.Nick mirage ;
        state := Nick_sended ;
        writer ()
      | Nick_sended ->
        send Cri.Protocol.Join [ Cri.Channel.of_string_exn "#mirage", None ] ;
        state := Join ;
        writer ()
      | Join -> writer ()
      | Joined -> writer ()
        (* send Cri.Protocol.Quit "Bye!" ;
           Lwt_switch.turn_off stop >|= close >>= writer *) in
  let rec reader () =
    recv () >>= fun v -> match v, !state with
    | Some (_, Cri.Protocol.Message (Notice, { msg; _ })), _ ->
      Logs.info (fun m -> m "%s" msg) ;
      reader ()
    | Some (_, Cri.Protocol.Message (RPL_LUSERCLIENT, _)), Join ->
      Logs.debug (fun m -> m "The server welcomed!") ;
      state := Joined ;
      Lwt.return_unit
    | Some _, _ -> reader ()
    | None, _ -> Lwt.return_unit in
  Lwt.join [ reader (); writer () ]

let host : [ `host ] Domain_name.t Mimic.value = Mimic.make ~name:"host"
let port : int Mimic.value = Mimic.make ~name:"port"
let inet_addr : Unix.inet_addr Mimic.value = Mimic.make ~name:"ipaddr"
let sockaddr, tcpip = Mimic.register ~name:"tcp/ip" (module Socket)

let gethostbyname host =
  match Unix.gethostbyname (Domain_name.to_string host) with
  | { Unix.h_addr_list; _ } when Array.length h_addr_list > 0 ->
    Lwt.return_some h_addr_list.(0)
  | _ -> Lwt.return_none
  | exception _ -> Lwt.return_none

let make inet_addr port =
  Lwt.return_some (Unix.ADDR_INET (inet_addr, port))

let ctx_of_uri uri =
  let ctx =
    Mimic.empty
    |> Mimic.fold sockaddr Mimic.Fun.[ req inet_addr; dft port 6665 ] ~k:make
    |> Mimic.fold inet_addr Mimic.Fun.[ req host; ] ~k:gethostbyname in
  let ctx = Mimic.add port (Option.value ~default:6665 (Uri.port uri)) ctx in
  let ctx = match Uri.host uri with
    | Some v ->
      ( try Mimic.add inet_addr (Unix.inet_addr_of_string v) ctx
        with _ -> Mimic.add host Domain_name.(host_exn (of_string_exn v)) ctx )
    | None -> ctx in
  ctx

let run ctx =
  let open Lwt.Infix in
  let stop = Lwt_switch.create () in
  let `Fiber th, recv, send, close = Cri_lwt.run ~stop ~ctx in
  Lwt.both
    (th >>= function
     | Ok _ as res -> Lwt.return res
     | Error _ as res ->
       Lwt_switch.turn_off stop >|= close >>= fun () ->
       Lwt.return res)
    (handler ~stop (ref Connected) recv send close) >>= function
  | Ok (), () -> Lwt.return_unit
  | Error err, () ->
    Fmt.epr "%a.\n%!" Cri_lwt.pp_error err ;
    Lwt.return_unit

let () =
  let ctx = ctx_of_uri (Uri.of_string Sys.argv.(1)) in
  Lwt_main.run (run ctx)
