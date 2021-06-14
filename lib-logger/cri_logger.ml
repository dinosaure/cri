let src = Logs.Src.create "cri-lwt"
module Log = (val Logs.src_log src : Logs.LOG)

module Atomic = struct
  type 'a t = { mutable v : 'a }

  let make v = {v}
  let get r = r.v
  let set r v = r.v <- v
end

type state =
  { step : step Atomic.t
  ; queue : (Cri.Protocol.prefix option * Cri.Protocol.message) Queue.t
  ; user : Cri.Protocol.user
  ; channel : Cri.Channel.t
  ; nickname : Cri.Nickname.t
  ; mutex : Lwt_mutex.t
  ; log : (Cri.Protocol.prefix option * Cri.Protocol.message) list -> unit Lwt.t
  ; tick : int64 }
and step =
  | Connected
  | Send_nick of [ `Done | `Errored ]
  | Send_user of [ `Done | `Errored ]
  | Join of [ `In_progress | `Done | `Errored ]
  | Log

(* XXX(dinosaure): see RFC 2812, 3.1 - Connection Registration
   The recommended order for a client to register is as follows:
   1. Pass message
   2. Nick message | 2. Service message
   3. User message
*)

let state ~user ~channel ~nickname ~tick log =
  { step= Atomic.make Connected
  ; queue= Queue.create ()
  ; user; channel; nickname
  ; mutex= Lwt_mutex.create ()
  ; log; tick; }

type join_error =
  [ `Need_more_params
  | `Invite_only_chan
  | `Channel_is_full
  | `No_such_channel
  | `Too_many_targets
  | `Banned_from_chan
  | `Bad_channel_key
  | `Bad_chan_mask
  | `Too_many_channels
  | `Unavailable_resource ]

type user_error =
  [ `Need_more_params
  | `Already_registered ]

type nick_error =
  [ `No_nickname_given
  | `Nickname_in_use
  | `Unavailable_resource
  | `Nick_collision
  | `Restricted ]

exception Join of join_error
exception User of user_error
exception Nick of nick_error

type error = [ join_error | user_error | nick_error ]

let pp_error : error Fmt.t = fun ppf -> function
  | `Need_more_params -> Fmt.pf ppf "Need more params"
  | `Invite_only_chan -> Fmt.pf ppf "Invite only chan"
  | `Channel_is_full -> Fmt.pf ppf "Channel is full"
  | `No_such_channel -> Fmt.pf ppf "No such channel"
  | `Too_many_targets -> Fmt.pf ppf "Too many targets"
  | `Banned_from_chan -> Fmt.pf ppf "Banned from chan"
  | `Bad_channel_key -> Fmt.pf ppf "Bad channel key"
  | `Bad_chan_mask -> Fmt.pf ppf "Bad chan mask"
  | `Too_many_channels -> Fmt.pf ppf "Too many channels"
  | `Unavailable_resource -> Fmt.pf ppf "Unavailable resource"
  | `Already_registered -> Fmt.pf ppf "Already registered"
  | `No_nickname_given -> Fmt.pf ppf "No nickname given"
  | `Nickname_in_use -> Fmt.pf ppf "Nickname in use"
  | `Nick_collision -> Fmt.pf ppf "Nick collision"
  | `Restricted -> Fmt.pf ppf "Restricted"

let rec writer state closed ({ Cri_lwt.send } as ssend) =
  let open Lwt.Infix in
  Lwt.pick [ closed; (Lwt.pause () >|= fun () -> `Continue) ] >>= function
  | `Closed -> Lwt.return_unit
  | `Continue -> match Atomic.get state.step with
    | Connected ->
      Log.debug (fun m -> m "Connected, send nick -> Send_nick `Done") ;
      send Cri.Protocol.Nick { Cri.Protocol.nick= state.nickname; hopcount= None; } ;
      Atomic.set state.step (Send_nick `Done) ;
      writer state closed ssend
    | Join `In_progress ->
      writer state closed ssend
    | Send_user `Done ->
      Log.debug (fun m -> m "Send_user `Done, send join -> Join `In_progress") ;
      send Cri.Protocol.Join [ state.channel, None ] ;
      Atomic.set state.step (Join `In_progress) ;
      writer state closed ssend
    | Send_nick `Done ->
      Log.debug (fun m -> m "Send_nick `Done, send user -> Send_user `Done") ;
      send Cri.Protocol.User state.user ;
      Atomic.set state.step (Send_user `Done) ;
      writer state closed ssend
    | Join `Done | Log ->
      Log.debug (fun m -> m "Join `Done | Log, terminate the application writer.") ;
      Atomic.set state.step Log ;
      Lwt.return_unit
    | Join `Errored | Send_user `Errored | Send_nick `Errored ->
      Log.warn (fun m -> m "Got a state error.") ;
      Lwt.return_unit (* TODO(dinosaure): retry? *)

let rec reader state recv ({ Cri_lwt.send } as ssend) =
  let open Lwt.Infix in
  Lwt.pause () >>= recv >>= fun v -> match v, Atomic.get state.step with
  | None, _ -> Lwt.return_unit
  | Some (_, Cri.Protocol.Message (RPL_TOPIC, (ch, topic))), Join (`In_progress | `Done) ->
    Log.info (fun m -> m "%a: %s" Cri.Channel.pp ch topic) ;
    Log.debug (fun m -> m "Start to save %a" Cri.Channel.pp ch) ;
    Atomic.set state.step (Join `Done) ;
    reader state recv ssend
  | Some (_, Cri.Protocol.Message (ERR_NONICKNAMEGIVEN, _)),
    (Send_nick `Done | Send_user `Done | Join `In_progress) ->
    Atomic.set state.step (Send_nick `Errored) ;
    reader state recv ssend
  | Some (_, Cri.Protocol.Message (Ping, v)), _ ->
    Log.debug (fun m -> m "Ping -> Pong") ;
    send Cri.Protocol.Pong v ;
    reader state recv ssend
  | Some (prefix, msg), Log ->
    Lwt_mutex.with_lock state.mutex @@ begin fun () ->
    Queue.push (prefix, msg) state.queue ; Lwt.return_unit end >>= fun () ->
    reader state recv ssend
  | Some _, _ -> reader state recv ssend

let rec drain queue = go [] queue
and go acc queue = match Queue.pop queue with
  | msg -> go (msg :: acc) queue
  | exception Queue.Empty -> acc

let rec tick ~sleep_ns state closed =
  let open Lwt.Infix in
  sleep_ns state.tick >>= fun () ->
  Lwt_mutex.with_lock state.mutex @@ begin fun () ->
  let lst = drain state.queue in Lwt.return lst end >>= fun lst ->
  Log.debug (fun m -> m "Call the logger with %d msg(s)." (List.length lst)) ;
  Lwt.async (fun () -> state.log lst) ;
  Lwt.pick [ closed; (Lwt.pause () >|= fun () -> `Continue) ] >>= function
  | `Closed -> Lwt.return_unit
  | `Continue -> tick ~sleep_ns state closed

let handler ~sleep_ns ~stop state recv send close =
  let open Lwt.Infix in
  let closed, u = Lwt.wait () in
  Lwt_switch.add_hook (Some stop)
    (fun () -> Lwt.wakeup_later u `Closed ; close () ; Lwt.return_unit) ;
  Lwt.catch
    (fun () ->
      Lwt.join [ reader state recv send
               ; writer state closed send
               ; tick ~sleep_ns state closed ] >>= fun () ->
      Lwt.return_ok ())
  @@ function
  | Join err -> Lwt_switch.turn_off stop >>= fun () -> Lwt.return_error (err :> error)
  | Nick err -> Lwt_switch.turn_off stop >>= fun () -> Lwt.return_error (err :> error)
  | User err -> Lwt_switch.turn_off stop >>= fun () -> Lwt.return_error (err :> error)
  | exn -> raise exn
