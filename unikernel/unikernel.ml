open Lwt.Infix

let msgs_to_string msgs =
  let rec go buf enc msgs = function
    | Cri.Encoder.Write { buffer; off; len; continue; } ->
      Buffer.add_substring buf buffer off len ;
      go buf enc msgs (continue len)
    | Cri.Encoder.Error _ -> assert false
    | Cri.Encoder.Done ->
      match msgs with
      | [] -> Buffer.contents buf
      | (p, Cri.Protocol.Message (t, v)) :: msgs ->
        go buf enc msgs Cri.Protocol.(encode enc ?prefix:p (send t v)) in
  match msgs with
  | [] -> ""
  | (p, Cri.Protocol.Message (t, v)) :: msgs ->
    let buf = Buffer.create 0x100 in
    let enc = Cri.Encoder.encoder () in
    go buf enc msgs Cri.Protocol.(encode enc ?prefix:p (send t v))

type config =
  { user : Cri.Protocol.user
  ; channel : Cri.Channel.t
  ; nickname : Cri.Nickname.t
  ; tick : int64 }

module Make
  (Pclock : Mirage_clock.PCLOCK)
  (Time : Mirage_time.S)
  (_ : Mirage_stack.V4V6)
  (_ : sig end)
  (_ : sig end) = struct
  module Store = Irmin_mirage_git.Mem.KV (Irmin.Contents.String)
  module Sync = Irmin.Sync (Store)

  let info () =
    let d, ps = Pclock.now_d_ps () in
    let ptime = Ptime.v (d, ps) in
    Irmin.Info.v ~date:(Int64.of_int d)
      ~author:(Key_gen.author ())
      (Fmt.str "log %a" Ptime.pp ptime)

  let config () =
    { user= { Cri.Protocol.username= "noisy-bot"
            ; hostname= Domain_name.of_string_exn "mirage.io"
            ; servername= Domain_name.of_string_exn "mirage.io"
            ; realname= "MirageOS noisy bot" }
    ; channel= Cri.Channel.of_string_exn (Key_gen.channel ())
    ; nickname= Cri.Nickname.of_string_exn (Key_gen.nickname ())
    ; tick= Int64.mul (Int64.of_int (Key_gen.tick ())) 1_000_000_000L }

  let save ~errored ctx uri msgs =
    let config = Irmin_mem.config () in
    Store.Repo.v config >>= Store.master >>= fun store ->
    let upstream = Store.remote ~ctx uri in
    Sync.pull ~depth:1 store upstream `Set >>= function
    | Error _ -> Lwt_switch.turn_off errored
    | Ok (`Head _ | `Empty as parent) ->
      let parents = match parent with
        | `Head commit -> [ commit ]
        | `Empty -> [] in
      let now = Pclock.now_d_ps () |> Ptime.v in
      let k = [ Ptime.to_rfc3339 ~space:false ~tz_offset_s:0 now ] in
      let msgs = msgs_to_string msgs in
      Store.set ~parents ~info store k msgs >>= function
      | Error _ -> Lwt_switch.turn_off errored
      | Ok () -> Sync.push store upstream >>= function
        | Ok _ -> Lwt.return_unit
        | Error _ -> Lwt_switch.turn_off errored

  let log ctx_irc ctx_git uri =
    let { user; channel; nickname; tick; } = config () in
    let stop = Lwt_switch.create () in
    let errored = Lwt_switch.create () in
    let error, wk = Lwt.wait () in
    Lwt_switch.add_hook (Some errored)
      (fun () -> Lwt.wakeup_later wk `Error ; Lwt_switch.turn_off stop) ;
    let state = Cri_logger.state ~user ~channel ~nickname ~tick (save ~errored ctx_git uri) in
    let `Fiber th, recv, send, close = Cri_lwt.run ~stop
      ~timeout:(fun () -> Time.sleep_ns 5_000_000_000L) ~ctx:ctx_irc in
    Lwt.both
      (th >>= fun res -> Lwt_switch.turn_off stop >|= close >>= fun () -> Lwt.return res)
      (Cri_logger.handler ~sleep_ns:Time.sleep_ns ~stop state recv send close) >>= function
    | Ok (), Ok () ->
      ( match Lwt.state error with
      | Lwt.Sleep -> Lwt.cancel error ; Lwt.return `Stop
      | Lwt.Fail _ | Lwt.Return _ -> Lwt.return `Retry )
    | Error _, _ | _, Error _ -> Lwt.return `Retry

  let start () () _stack ctx_irc ctx_git =
    let ctx_irc = Mimic.merge ctx_irc (Cri_mirage.ctx_of_uri (Uri.of_string (Key_gen.irc ()))) in
    let rec infinite () = log ctx_irc ctx_git (Key_gen.remote ()) >>= function
      | `Retry -> infinite ()
      | `Stop -> Lwt.return_unit in
    infinite ()
end
