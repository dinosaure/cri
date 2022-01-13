[@@@warning "-8"]

type nick = { nick : Nickname.t; hopcount : int option; }

type user = { username : string
            ; mode : int
            ; realname : string }

type server = { servername : [ `raw ] Domain_name.t
              ; hopcount : int
              ; info : string }

type oper = { user : string; password : string; }

type notice = { dsts : Destination.t list; msg : string; }

type 'a prettier =
  [ `Pretty of 'a | `String of string | `None ]

type welcome = { nick : string; user : string; host : [ `raw ] Domain_name.t; }

type discover = { users : int; services : int; servers : int; }

type reply =
  { numeric : int
  ; params : string list * string option }

type user_mode =
  { nickname : Nickname.t
  ; modes : User_mode.modes }

type channel_mode =
  { channel : Channel.t
  ; modes : (Channel_mode.modes * string option) list }

type names =
  { channel : Channel.t
  ; kind : [ `Secret | `Private | `Public ]
  ; names : ([ `Operator | `Voice | `None ] * Nickname.t) list }

type host =
  [ `Host of [ `raw ] Domain_name.t
  | `Ip6 of Ipaddr.V6.t ]

let pp_nick ppf { nick; hopcount; } = match hopcount with
  | Some hopcount -> Fmt.pf ppf "%a (hopcount: %d)" Nickname.pp nick hopcount
  | None -> Nickname.pp ppf nick

let pp_user ppf { username; mode; realname; } =
  Fmt.pf ppf "%s %d (%s)"
    username mode realname

let pp_server ppf { servername; hopcount; info; } =
  Fmt.pf ppf "%a:%d (%s)" Domain_name.pp servername hopcount info

let pp_oper ppf { user; password= _; } =
  Fmt.pf ppf "%s" user

let pp_notice ppf { dsts; msg; } =
  Fmt.pf ppf "%a: %s" Fmt.(Dump.list Destination.pp) dsts msg

let pp_prettier pp ppf = function
  | `Pretty v -> pp ppf v
  | `String v -> Fmt.string ppf v
  | `None -> ()

let pp_welcome ppf { nick; user; host; } =
  Fmt.pf ppf "%s!%s@%a" nick user Domain_name.pp host

let pp_discover ppf { users; services; servers; } =
  Fmt.pf ppf "users:%d, services: %d, servers:%d" users services servers

let pp_reply ppf { numeric; params= ps, r; } = match ps, r with
  | _ :: _, Some r -> Fmt.pf ppf "%03d %a :%s" numeric Fmt.(list ~sep:(any "@ ") string) ps r
  | _ :: _, None -> Fmt.pf ppf "%03d %a" numeric Fmt.(list ~sep:(any "@ ") string) ps
  | [], Some r -> Fmt.pf ppf "%03d :%s" numeric r
  | [], None -> Fmt.pf ppf "%03d" numeric

let pp_user_mode ppf { nickname; modes; } =
  Fmt.pf ppf "%a %a" Nickname.pp nickname User_mode.pp modes

let pp_names ppf { channel; kind; names; } =
  let pp_name ppf (k, nickname) = match k with
    | `Operator -> Fmt.pf ppf "@%a" Nickname.pp nickname
    | `Voice -> Fmt.pf ppf "+%a" Nickname.pp nickname
    | `None -> Nickname.pp ppf nickname in
  match kind with
  | `Secret -> Fmt.pf ppf "@%a %a" Channel.pp channel Fmt.(Dump.list pp_name) names
  | `Private -> Fmt.pf ppf "*%a %a" Channel.pp channel Fmt.(Dump.list pp_name) names
  | `Public -> Fmt.pf ppf "=%a %a" Channel.pp channel Fmt.(Dump.list pp_name) names

let pp_host ppf = function
  | `Host v -> Domain_name.pp ppf v
  | `Ip6 v -> Ipaddr.V6.pp ppf v

type prefix =
  | Server of host
  | User of { name : Nickname.t
            ; user : string option
            ; host : host option }

let pp_prefix ppf (prefix : prefix) = match prefix with
  | Server host -> pp_host ppf host
  | User { name; user; host; } ->
    Fmt.pf ppf "%a%a%a"
      Nickname.pp name
      Fmt.(option ((const string "!") ++ string)) user
      Fmt.(option ((const string "@") ++ pp_host)) host

module Fun = struct
  type ('k, 'res) args =
    | [] : ('res, 'res) args
    | ( :: ) : 'a arg * ('k, 'res) args -> ('a -> 'k, 'res) args
  and 'v arg = ..

  type 'v arg += Int : int arg
  type 'v arg += String : string arg
end

type 'a t =
  | Pass : string t
  | Nick : nick t
  | User : user t
  | Server : server t
  | Oper : oper t
  | Quit : string t
  | SQuit : ([ `raw ] Domain_name.t * string) t
  | Join : (Channel.t * string option) list t
  | Notice : notice t
  | User_mode : user_mode t
  | Channel_mode : channel_mode t
  | Privmsg : (Destination.t list * string) t
  | Ping : ([ `raw ] Domain_name.t option * [ `raw ] Domain_name.t option) t
  | Pong : ([ `raw ] Domain_name.t option * [ `raw ] Domain_name.t option) t
  | Part : (Channel.t list * string option) t
  | Topic : (Channel.t * string option) t
  | Error : string option t
  | RPL_WELCOME : welcome prettier t
  | RPL_LUSERCLIENT : discover prettier t
  | RPL_YOURHOST : ([ `raw ] Domain_name.t * string) prettier t
  | RPL_CREATED : Ptime.t prettier t
  | RPL_MYINFO : string option (* TODO *) t
  | RPL_BOUNCE : string option (* TODO *) t
  | RPL_LUSEROP : int prettier t
  | RPL_LUSERUNKNOWN : int prettier t
  | RPL_LUSERCHANNELS : int prettier t
  | RPL_LUSERME : (int * int) prettier t
  | RPL_MOTDSTART : string option t
  | RPL_MOTD : string prettier t
  | RPL_ENDOFMOTD : string option t
  | RPL_TOPIC : (Channel.t * string) t
  | RPL_NOTOPIC : Channel.t t
  | RPL_NAMREPLY : names t
  | RPL_ENDOFNAMES : Channel.t t
  | ERR_NONICKNAMEGIVEN : unit t
  | RPL : reply t

type command = Command : 'a t -> command
type message = Message : 'a t * 'a -> message

let command_of_line (_, command, parameters) = match String.lowercase_ascii command with
  | "pass" -> Ok (Command Pass)
  | "nick" -> Ok (Command Nick)
  | "user" -> Ok (Command User)
  | "server" -> Ok (Command Server)
  | "oper" -> Ok (Command Oper)
  | "quit" -> Ok (Command Quit)
  | "squit" -> Ok (Command SQuit)
  | "notice" -> Ok (Command Notice)
  | "join" -> Ok (Command Join)
  | "mode" -> ( match parameters with
              | v :: _, _ ->
                if Channel.is v
                then Ok (Command Channel_mode)
                else if Nickname.is v
                then Ok (Command User_mode)
                else Rresult.R.error_msgf "Unknown command: %S" command
              | _ -> Rresult.R.error_msgf "Unknown command: %S" command )
  | "privmsg" -> Ok (Command Privmsg)
  | "ping" -> Ok (Command Ping)
  | "pong" -> Ok (Command Pong)
  | "part" -> Ok (Command Part)
  | "topic" -> Ok (Command Topic)
  | "error" -> Ok (Command Error)
  | "001" -> Ok (Command RPL_WELCOME)
  | "002" -> Ok (Command RPL_YOURHOST)
  | "003" -> Ok (Command RPL_CREATED)
  | "004" -> Ok (Command RPL_MYINFO)
  | "005" -> Ok (Command RPL_BOUNCE)
  | "251" -> Ok (Command RPL_LUSERCLIENT)
  | "252" -> Ok (Command RPL_LUSEROP)
  | "253" -> Ok (Command RPL_LUSERUNKNOWN)
  | "254" -> Ok (Command RPL_LUSERCHANNELS)
  | "255" -> Ok (Command RPL_LUSERME)
  | "250" | "265" | "266" | "333" -> Ok (Command RPL)
  | "375" -> Ok (Command RPL_MOTDSTART)
  | "372" -> Ok (Command RPL_MOTD)
  | "376" -> Ok (Command RPL_ENDOFMOTD)
  | "331" -> Ok (Command RPL_NOTOPIC)
  | "332" -> Ok (Command RPL_TOPIC)
  | "353" -> Ok (Command RPL_NAMREPLY)
  | "366" -> Ok (Command RPL_ENDOFNAMES)
  | "431" -> Ok (Command ERR_NONICKNAMEGIVEN)
  | _ -> Rresult.R.error_msgf "Unknown command: %S" command

type send = Send : 'a t * 'a -> send

type 'a recv =
  | Recv : 'a t -> (prefix option * 'a) recv
  | Any : (prefix option * message) recv
  | Many : (prefix option * message) list recv

let to_prefix : prefix option -> _ = function
  | Some (User { name; user; host= Some (`Host host); }) ->
    Some (`User (Nickname.to_string name, user, Some (`Host (Domain_name.to_string host))))
  | Some (User { name; user; host= Some (`Ip6 host); }) ->
    Some (`User (Nickname.to_string name, user, Some (`Ip6 host)))
  | Some (User { name; user; host= None; }) ->
    Some (`User (Nickname.to_string name, user, None))
  | Some (Server (`Host v)) -> Some (`Server (`Host (Domain_name.to_string v)))
  | Some (Server (`Ip6 v)) -> Some (`Server (`Ip6 v))
  | None -> None

let of_prettier pp = function
  | `Pretty v -> Some (Fmt.str "%a" pp v)
  | `String str -> Some str
  | `None -> None

let identity x = x

let scanner_of_reply
  : type k r. r prettier t -> (k, r) Fun.args -> ((k, _, _, _, _, r) format6 * k) option
  = fun w args -> match w, args with
  | RPL_LUSERCHANNELS, Fun.[ Int ] ->
    Some ("%d channel(s) formed" ^^ "", identity)
  | RPL_LUSERME, Fun.[ Int; Int ]  ->
    Some ("I have %d clients and %d servers" ^^ "", (fun a b -> (a, b)))
  | RPL_MOTD, Fun.[ String ] ->
    Some ("- %s" ^^ "", identity)
  | _ -> None

let to_prettier
  : type a. a prettier t -> string list * string option -> a prettier
  = fun t params -> match t, params with
    | RPL_LUSERCHANNELS, ([], Some msg) ->
      let scanner = scanner_of_reply t Fun.[ Int ] in
      ( match Option.map (fun (scanner, k) -> Scanf.sscanf msg scanner k) scanner with
      | Some v -> `Pretty v | None -> `String msg | exception _ -> `String msg)
    | RPL_LUSERME, ([], Some msg) ->
      let scanner = scanner_of_reply t Fun.[ Int; Int ] in
      ( match Option.map (fun (scanner, k) -> Scanf.sscanf msg scanner k) scanner with
      | Some v -> `Pretty v | None -> `String msg | exception _ -> `String msg)
    | RPL_MOTD, ([], Some msg) ->
      let scanner = scanner_of_reply t Fun.[ String ] in
      ( match Option.map (fun (scanner, k) -> Scanf.sscanf msg scanner k) scanner with
      | Some v -> `Pretty v | None -> `String msg | exception _ -> `String msg)
    | _, (_, Some msg) -> `String msg
    | _, (_, None) -> `None

let to_line
  : type a. ?prefix:prefix -> a t -> a -> Encoder.t
  = fun ?prefix w v -> match w, v with
    | Pass, v -> to_prefix prefix, "pass", ([ v ], None)
    | Nick, { nick; hopcount= None; } ->
      to_prefix prefix, "nick", ([ Nickname.to_string nick ], None)
    | Nick, { nick; hopcount= Some hopcount; } ->
      to_prefix prefix, "nick", ([ Nickname.to_string nick; string_of_int hopcount ], None)
    | User, { username; mode; realname; } ->
      to_prefix prefix, "user", ([ username; string_of_int mode; "*"; ], Some realname)
    | Server, { servername; hopcount; info; } ->
      let servername = Domain_name.to_string servername in
      to_prefix prefix, "server", ([ servername; string_of_int hopcount; ], Some info)
    | Oper, { user; password; } ->
      to_prefix prefix, "oper", ([ user; password; ], None)
    | Quit, msg ->
      to_prefix prefix, "quit", ([], Some msg)
    | SQuit, (server, msg) ->
      let server = Domain_name.to_string server in
      to_prefix prefix, "squit", ([ server ], Some msg)
    | Notice, { dsts; msg; } ->
      let dsts = List.map Destination.to_string dsts in
      let dsts = String.concat "," dsts in
      to_prefix prefix, "notice", ([ dsts ], Some msg)
    | Join, channels ->
      let channels, keys = List.split channels in
      let keys, _ = List.partition Option.is_some keys in
      let keys = List.map Option.get keys in
      let channels = List.map Channel.to_string channels in
      let channels = String.concat "," channels in
      let keys = String.concat "," keys in
      to_prefix prefix, "join", ([ channels; keys; ], None)
    | User_mode, { nickname; modes; } ->
      to_prefix prefix, "mode", ([ Nickname.to_string nickname ], Some (User_mode.to_string modes))
    | Channel_mode, { channel; modes; } ->
      let rec parameters acc = function
        | [] -> List.rev acc
        | (modes, None) :: tl -> parameters (Channel_mode.to_string modes :: acc) tl
        | (modes, Some v) :: tl -> parameters (v :: Channel_mode.to_string modes :: acc) tl in
      to_prefix prefix, "mode", (parameters [ Channel.to_string channel ] modes, None)
    | Privmsg, (dsts, msg) ->
      let dsts = List.map Destination.to_string dsts in
      let dsts = String.concat "," dsts in
      to_prefix prefix, "privmsg", ([ dsts ], Some msg)
    | Ping, (src, dst) ->
      ( match src, dst with
      | Some src, None -> to_prefix prefix, "ping", ([ Domain_name.to_string src ], None)
      | None, Some dst -> to_prefix prefix, "ping", ([], Some (Domain_name.to_string dst))
      | Some src, Some dst ->
        to_prefix prefix, "ping", ([ Domain_name.to_string src; Domain_name.to_string dst ], None)
      | None, None -> assert false (* TODO *) )
    | Pong, (src, dst) ->
      ( match src, dst with
      | Some src, None -> to_prefix prefix, "pong", ([ Domain_name.to_string src ], None)
      | None, Some dst -> to_prefix prefix, "pong", ([], Some (Domain_name.to_string dst))
      | Some src, Some dst ->
        to_prefix prefix, "pong", ([ Domain_name.to_string src; Domain_name.to_string dst ], None)
      | None, None -> assert false (* TODO *) )
    | Part, (channels, msg) ->
      let channels = String.concat "," (List.map Channel.to_string channels) in
      to_prefix prefix, "part", ([ channels ], msg)
    | Topic, (channel, topic) ->
      to_prefix prefix, "topic", ([ Channel.to_string channel ], topic)
    | Error, msg ->
      to_prefix prefix, "error", ([], msg)
    | RPL_WELCOME, v ->
      let param = match v with
        | `Pretty { nick; _ } -> [ nick ]
        | _ -> [] in
      let pp ppf { nick; user; host; } =
        Fmt.pf ppf "Welcome to the Cri Internet Relay Network %s!%s@%a"
          nick user Domain_name.pp host in
      to_prefix prefix, "001", (param, of_prettier pp v)
    | RPL_YOURHOST, v ->
      let pp ppf (servername, version) =
        Fmt.pf ppf "Your host is %a, running version %s"
          Domain_name.pp servername version in
      to_prefix prefix, "002", ([], of_prettier pp v)
    | RPL_CREATED, v ->
      let pp = Ptime.pp_rfc3339 ~space:false () in (* XXX(dinosaure): time-zone? *)
      to_prefix prefix, "003", ([], of_prettier pp v)
    | RPL_MYINFO, msg ->
      to_prefix prefix, "004", ([], msg)
    | RPL_BOUNCE, msg ->
      to_prefix prefix, "005", ([], msg)
    | RPL_LUSERCLIENT, v ->
      let pp ppf { users; services; servers; } =
        Fmt.pf ppf "There are %d users and %d services and %d servers"
          users services servers in
      to_prefix prefix, "251", ([], of_prettier pp v)
    | RPL_LUSEROP, v ->
      let pp ppf operators = Fmt.pf ppf "%d operator(s) online" operators in
      to_prefix prefix, "252", ([], of_prettier pp v)
    | RPL_LUSERUNKNOWN, v ->
      let pp ppf unknown_connections =
        Fmt.pf ppf "%d unknown connection(s)" unknown_connections in
      to_prefix prefix, "253", ([], of_prettier pp v)
    | RPL_LUSERME, v ->
      let pp ppf (clients, servers) =
        Fmt.pf ppf "I have %d clients and %d servers" clients servers in
      to_prefix prefix, "255", ([], of_prettier pp v)
    | RPL_MOTDSTART, msg ->
      to_prefix prefix, "375", ([], msg)
    | RPL_MOTD, v ->
      let pp ppf msg = Fmt.pf ppf "- %s" msg in
      to_prefix prefix, "372", ([], of_prettier pp v)
    | RPL_ENDOFMOTD, msg ->
      to_prefix prefix, "376", ([], msg)
    | RPL_TOPIC, (channel, topic) ->
      to_prefix prefix, "332", ([ Channel.to_string channel ], Some topic)
    | RPL_NOTOPIC, channel ->
      to_prefix prefix, "331", ([ Channel.to_string channel ], None)
    | RPL_NAMREPLY, { channel; kind; names; } ->
      let kind_to_string = function
        | `Secret -> "@" | `Private -> "*" | `Public -> "=" in
      let params = [ Channel.to_string channel; kind_to_string kind ] in 
      let pp_name ppf = function
        | `None, nickname -> Nickname.pp ppf nickname
        | `Operator, nickname -> Fmt.pf ppf "@%a" Nickname.pp nickname
        | `Voice, nickname -> Fmt.pf ppf "+%a" Nickname.pp nickname in
      let msg = Fmt.str "%a" Fmt.(list ~sep:(const string " ") pp_name) names in
      to_prefix prefix, "353", (params, Some msg)
    | RPL_ENDOFNAMES, channel ->
      to_prefix prefix, "366", ([ Channel.to_string channel ], None)
    | RPL, { numeric; params; } ->
      to_prefix prefix, (Fmt.str "%03d" numeric), params
    | ERR_NONICKNAMEGIVEN, () ->
      to_prefix prefix, "431", ([], Some "No nickname given")

let pp_message ppf (Message (t, v)) = match t with
  | Pass -> Fmt.pf ppf "pass %s" v
  | Nick -> Fmt.pf ppf "nick %a" pp_nick v
  | User -> Fmt.pf ppf "user %a" pp_user v
  | Server -> Fmt.pf ppf "server %a" pp_server v
  | Oper -> Fmt.pf ppf "oper %a" pp_oper v
  | Quit -> Fmt.pf ppf "quit %S" v
  | SQuit -> Fmt.pf ppf "squit %a %S" Domain_name.pp (fst v) (snd v)
  | Join -> Fmt.pf ppf "join %a" Fmt.(Dump.list (Dump.pair Channel.pp (option (fmt "%S")))) v
  | _ ->
    let _prefix, command, (ps, v) = to_line t v in
    match v with
    | Some v -> Fmt.pf ppf "%s %a :%s" command Fmt.(list ~sep:(any "@ ") string) ps v
    | None -> Fmt.pf ppf "%s %a" command Fmt.(list ~sep:(any "@ ") string) ps

let apply_keys channels keys =
  let rec go acc channels keys = match channels with
    | [] -> List.rev acc
    | channel :: channels -> match keys with
      | key :: keys -> go ((channel, Some key) :: acc) channels keys
      | [] -> go ((channel, None) :: acc) channels [] in
  go [] channels keys

let kind_of_string_exn = function
  | "=" -> `Public
  | "*" -> `Private
  | "@" -> `Secret
  | str -> Fmt.invalid_arg "Invalid type of channel: %S" str

let rec name_of_string_exn str =
  if str = "" then Fmt.invalid_arg "Empty nickname" ;
  match str.[0] with
  | '@' -> `Operator, Nickname.of_string_exn (chop str)
  | '+' -> `Voice, Nickname.of_string_exn (chop str)
  | _   -> `None, Nickname.of_string_exn str
and chop str = String.sub str 1 (String.length str - 1)

let rec of_line
  : type a. a recv
         -> Decoder.t
         -> (a, [ `Invalid_parameters | `Invalid_command | `Invalid_reply ]) result
  = fun w ((prefix, command, vs) as line) ->
  let prefix : prefix option = match prefix with
    | None -> None
    | Some (`Server (`Ip6 v)) -> Some (Server (`Ip6 v))
    | Some (`Server (`Host v)) -> Some (Server (`Host (Domain_name.of_string_exn v)))
    | Some (`User (name, user, Some (`Ip6 v))) ->
      let name = Nickname.of_string_exn name in
      Some (User { name; user; host= Some (`Ip6 v); })
    | Some (`User (name, user, Some (`Host host))) ->
      let name = Nickname.of_string_exn name in
      let host = Domain_name.of_string_exn host in
      Some (User { name; user; host= Some (`Host host); })
    | Some (`User (name, user, None)) ->
      let name = Nickname.of_string_exn name in
      Some (User { name; user; host= None; }) in
  match w, String.lowercase_ascii command, vs with
  | Recv Pass, "pass", ([ pass ], _) -> Ok (prefix, pass)
  | Recv Nick, "nick", ([ nick ], _) ->
    ( match Nickname.of_string nick with
    | Ok nick -> Ok (prefix, { nick; hopcount= None; })
    | Error _ -> Error `Invalid_parameters )
  | Recv Nick, "nick", ([], Some nick) ->
    ( try Ok (prefix, { nick= Nickname.of_string_exn nick; hopcount= None })
      with _ -> Error `Invalid_parameters )
  | Recv Nick, "nick", ([ nick; hopcount; ], _) ->
    ( try Ok (prefix, { nick= Nickname.of_string_exn nick; hopcount= Some (int_of_string hopcount) })
      with _ -> Error `Invalid_parameters )
  | Recv User, "user", ([ username; mode; _; ], realname) ->
    let realname = Option.value ~default:"" realname in
    ( try let mode = int_of_string mode in
          Ok (prefix, { username; mode; realname; })
      with _ -> Error `Invalid_parameters )
  | Recv Server, "server", ([ servername; hopcount; ], Some info) ->
    ( try let servername = Domain_name.of_string_exn servername in
          let hopcount = int_of_string hopcount in
          Ok (prefix, { servername; hopcount; info; })
      with _ -> Error `Invalid_parameters )
  | Recv Oper, "oper", ([ user; password; ], _) ->
    Ok (prefix, { user; password; })
  | Recv Quit, "quit", ([], Some msg) -> Ok (prefix, msg)
  | Recv SQuit, "squit", ([ server ], Some msg) ->
    ( try let server = Domain_name.of_string_exn server in
          Ok (prefix, (server, msg))
      with _ -> Error `Invalid_parameters )
  | Recv Notice, "notice", (dsts, Some msg) ->
    ( try let dsts = String.concat "" dsts in
          let dsts = Destination.of_string_exn dsts in
          Ok (prefix, { dsts; msg; })
      with _ -> Error `Invalid_parameters )
  | Recv Join, "join", ([ channels; keys; ], _) ->
    let channels = Astring.String.cuts ~sep:"," channels in
    let keys = Astring.String.cuts ~sep:"," keys in
    ( try let channels = List.map Channel.of_string_exn channels in
          let channels = apply_keys channels keys in
          Ok (prefix, channels)
      with _ -> Error `Invalid_parameters )
  | Recv Join, "join", ([ channels ], _) ->
    let channels = Astring.String.cuts ~sep:"," channels in
    ( try let channels = List.map Channel.of_string_exn channels in
          Ok (prefix, List.map (fun v -> v, None) channels)
      with _ -> Error `Invalid_parameters )
  | Recv User_mode, "mode", ([ nickname ], Some modes) ->
    ( match Nickname.of_string nickname, User_mode.of_string modes with
    | Ok nickname, Ok modes -> Ok (prefix, { nickname; modes; })
    | _ -> Error `Invalid_parameters )
  | Recv User_mode, "mode", ([ nickname; modes; ], None) ->
    ( match Nickname.of_string nickname, User_mode.of_string modes with
    | Ok nickname, Ok modes -> Ok (prefix, { nickname; modes; })
    | _ -> Error `Invalid_parameters )
  | Recv Channel_mode, "mode", (channel :: parameters, _) ->
    ( try let channel = Channel.of_string_exn channel in
          let rec modes acc = function
            | [] -> List.rev acc
            | m :: (parameters :: tl1 as tl0) ->
              ( match Channel_mode.of_string ~ignore:false parameters with
              | Ok _ -> modes ((Channel_mode.of_string_exn m, None) :: acc) tl0
              | Error _ -> modes ((Channel_mode.of_string_exn m, Some parameters) :: acc) tl1 )
            | [ m ] -> List.rev ((Channel_mode.of_string_exn m, None) :: acc) in
          Ok (prefix, { channel; modes= modes [] parameters })
      with _ -> Error `Invalid_parameters )
  | Recv Privmsg, "privmsg", ([ dsts ], msg) ->
    ( try let dsts = Destination.of_string_exn dsts in
          let msg = Option.value ~default:"" msg in
          Ok (prefix, (dsts, msg))
      with _ -> Error `Invalid_parameters )
  | Recv Ping, "ping", params ->
    ( match params with
    | [ src; dst ], None | [ src ], Some dst ->
      ( try let src = Domain_name.of_string_exn src in
            let dst = Domain_name.of_string_exn dst in
            Ok (prefix, (Some src, Some dst))
        with _ -> Error `Invalid_parameters )
    | [ src ], None ->
      ( try let src = Domain_name.of_string_exn src in
            Ok (prefix, (Some src, None))
        with _ -> Error `Invalid_parameters )
    | [], Some dst ->
      ( try let dst = Domain_name.of_string_exn dst in
            Ok (prefix, (None, Some dst))
        with _ -> Error `Invalid_parameters )
    | _ -> Error `Invalid_parameters )
  | Recv Pong, "pong", params ->
    ( match params with
    | [ src; dst ], None | [ src ], Some dst ->
      ( try let src = Domain_name.of_string_exn src in
            let dst = Domain_name.of_string_exn dst in
            Ok (prefix, (Some src, Some dst))
        with _ -> Error `Invalid_parameters )
    | [ src ], None ->
      ( try let src = Domain_name.of_string_exn src in
            Ok (prefix, (Some src, None))
        with _ -> Error `Invalid_parameters )
    | [], Some dst ->
      ( try let dst = Domain_name.of_string_exn dst in
            Ok (prefix, (None, Some dst))
        with _ -> Error `Invalid_parameters )
    | _ -> Error `Invalid_parameters )
  | Recv Part, "part", (channels, msg) ->
    ( try
        let channels = String.concat "" channels in
        let channels = Astring.String.cuts ~sep:"," channels in
        let channels = List.map Channel.of_string_exn channels in
        Ok (prefix, (channels, msg))
      with _ -> Error `Invalid_parameters )
  | Recv Topic, "topic", ([ channel ], topic) ->
    ( match Channel.of_string channel with
    | Ok channel -> Ok (prefix, (channel, topic))
    | Error _ -> Error `Invalid_parameters )
  | Recv Error, "error", (_, msg) -> Ok (prefix, msg)
  | Recv RPL_WELCOME,       "001", params -> Ok (prefix, to_prettier RPL_WELCOME       params)
  | Recv RPL_YOURHOST,      "002", params -> Ok (prefix, to_prettier RPL_YOURHOST      params)
  | Recv RPL_CREATED,       "003", params -> Ok (prefix, to_prettier RPL_CREATED       params)
  | Recv RPL_MYINFO,        "004", (_, msg) -> Ok (prefix, msg)
  | Recv RPL_BOUNCE,        "005", (_, msg) -> Ok (prefix, msg)
  | Recv RPL_LUSERCLIENT,   "251", params -> Ok (prefix, to_prettier RPL_LUSERCLIENT   params)
  | Recv RPL_LUSEROP,       "252", params -> Ok (prefix, to_prettier RPL_LUSEROP       params)
  | Recv RPL_LUSERUNKNOWN,  "253", params -> Ok (prefix, to_prettier RPL_LUSERUNKNOWN  params)
  | Recv RPL_LUSERCHANNELS, "254", params -> Ok (prefix, to_prettier RPL_LUSERCHANNELS params)
  | Recv RPL_LUSERME,       "255", params -> Ok (prefix, to_prettier RPL_LUSERME       params)
  | Recv RPL_MOTDSTART,     "375", (_, msg) -> Ok (prefix, msg)
  | Recv RPL_MOTD,          "372", params -> Ok (prefix, to_prettier RPL_MOTD          params)
  | Recv RPL_ENDOFMOTD,     "376", (_, msg) -> Ok (prefix, msg)
  | Recv RPL_TOPIC,         "332", ((_ :: _ as params), Some topic) ->
    let channel = List.hd (List.rev params) in
    ( try Ok (prefix, (Channel.of_string_exn channel, topic))
      with _ -> Error `Invalid_parameters )
  | Recv RPL_NOTOPIC,       "331", ([ channel ], _) ->
    ( try Ok (prefix, Channel.of_string_exn channel)
      with _ -> Error `Invalid_parameters )
  | Recv RPL_NAMREPLY,      "353", ((_ :: _ as params), Some names) ->
    ( match List.rev params with
    | channel :: kind :: _ ->
      ( try let channel = Channel.of_string_exn channel in
            let kind = kind_of_string_exn kind in
            let names = List.map name_of_string_exn (Astring.String.cuts ~sep:" " names) in
            Ok (prefix, { channel; kind; names; })
        with _ -> Error `Invalid_parameters )
    | _ -> Error `Invalid_parameters )
  | Recv RPL_ENDOFNAMES,    "366", ((_ :: _ as params), _) ->
    ( try let channel = Channel.of_string_exn (List.hd (List.rev params) ) in
          Ok (prefix, channel)
      with _ -> Error `Invalid_parameters )
  | Recv RPL, numeric, params ->
    ( try Ok (prefix, { numeric= int_of_string numeric; params; })
      with _ -> Error `Invalid_reply )
  | Recv ERR_NONICKNAMEGIVEN, "431", _ -> Ok (prefix, ())
  | Any, _, _ ->
    ( match command_of_line line with
    | Error _ -> Error `Invalid_command
    | Ok (Command c) -> match of_line (Recv c) line with
      | Ok (prefix, v) -> Ok (prefix, Message (c, v))
      | Error _ as err -> err )
  | Many, _, _ -> failwith "Impossible to get many lines from [of_line]"
    (* XXX(dinosaure): should never occur! *)
  | _ -> Error `Invalid_command

let prefix ?user ?host name : prefix =
  User { name; user; host= Option.map (fun v -> `Host v) host; }

let send : type a. a t -> a -> send
  = fun w v -> Send (w, v)
let recv : type a. a t -> (prefix option * a) recv
  = fun w -> Recv w
let any = Any
let many = Many

let encode
  : ?prefix:prefix -> Encoder.encoder -> send -> [> Encoder.error ] Encoder.state
  = fun ?prefix encoder (Send (w, v)) ->
    let line = to_line ?prefix w v in
    Encoder.encode_line (fun () -> Encoder.Done) encoder line

type error = [ `Invalid_command | `Invalid_parameters | `Invalid_reply | Decoder.error ]

let pp_error ppf = function
  | `Invalid_command -> Fmt.string ppf "Invalid command"
  | `Invalid_parameters -> Fmt.string ppf "Invalid parameters"
  | `Invalid_reply -> Fmt.string ppf "Invalid reply"
  | #Decoder.error as err -> Decoder.pp_error ppf err

let decode
  : type a. Decoder.decoder -> a recv -> (a, [> error ]) Decoder.state
  = fun decoder -> function
  | Many ->
    let rec k acc line decoder = match of_line Any line with
      | Ok x ->
        Decoder.junk_eol decoder ;
        if Decoder.at_least_one_line decoder
        then Decoder.peek_line ~k:(k (x :: acc)) decoder
             |> Decoder.reword_error (fun err -> (err :> error))
        else Decoder.return decoder (List.rev (x :: acc))
      | Error err -> Decoder.leave_with decoder (err :> error) in
    Decoder.peek_line ~k:(k []) decoder
    |> Decoder.reword_error (fun err -> (err :> error))
  | w ->
    let k line decoder = match of_line w line with
      | Ok v -> Decoder.junk_eol decoder ; Decoder.return decoder v
      | Error err -> Decoder.leave_with decoder (err :> error) in
    Decoder.peek_line ~k decoder
    |> Decoder.reword_error (fun err -> (err :> error))
