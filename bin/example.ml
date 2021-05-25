let () = Printexc.record_backtrace true

let hostname = Domain_name.of_string_exn (Unix.gethostname ())

let romain_calascibetta =
  { Cri.Protocol.username= "romain.calascibetta"
  ; Cri.Protocol.hostname
  ; Cri.Protocol.servername= Domain_name.of_string_exn "*"
  ; Cri.Protocol.realname= "Romain Calascibetta" }

let handler ~stop recv send =
  let open Lwt.Infix in
  let close, u = Lwt.wait () in
  let rec writer () =
    Lwt.pick [ close; (Lwt.pause () >|= fun () -> `Continue) ] >>= function
    | `Closed ->
      Fmt.epr ">>> Closed.\n%!" ;
      send (Some (None, Cri.Protocol.(Send (User, romain_calascibetta)))) ;
      send (Some (None, Cri.Protocol.(Send (Quit, "Bye!")))) ;
      send None ;
      Lwt.return_unit
    | `Continue -> writer () in
  let rec reader () =
    recv () >>= function
    | Some (_, Cri.Protocol.Message (Notice, { msg; _ })) ->
      Fmt.epr ">>> %S.\n%!" msg ;
      reader ()
    | Some _ -> reader ()
    | None -> Lwt.return_unit in
  let timer () =
    Lwt_unix.sleep 1.5 >>= fun () ->
    Fmt.epr ">>> Stop!\n%!" ;
    Lwt_switch.turn_off stop >>= fun () ->
    Lwt.wakeup_later u `Closed ;
    Lwt.return_unit in
  Lwt.join [ writer (); reader (); timer () ]

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

let () =
  let ctx = ctx_of_uri (Uri.of_string Sys.argv.(1)) in
  let stop = Lwt_switch.create () in
  let `Fiber th, recv, send = Cri_lwt.run ~stop ~ctx in
  match Lwt_main.run (Lwt.both th (handler ~stop recv send)) with
  | Ok (), () -> ()
  | Error err, () -> Fmt.epr "%a.\n%!" Cri_lwt.pp_error err
