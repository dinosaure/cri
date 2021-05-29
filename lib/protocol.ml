[@@@warning "-8"]

type nick = { nick : Nickname.t; hopcount : int option; }

type user = { username : string
            ; hostname : [ `raw ] Domain_name.t
            ; servername : [ `raw ] Domain_name.t
            ; realname : string }

type server = { servername : [ `raw ] Domain_name.t
              ; hopcount : int
              ; info : string }

type oper = { user : string; password : string; }

type notice = { nickname : string; msg : string; }

type 'a prettier =
  [ `Pretty of 'a | `String of string | `None ]

type welcome = { nick : string; user : string; host : [ `raw ] Domain_name.t; }

type discover = { users : int; services : int; servers : int; }

type reply =
  { numeric : int
  ; params : string list * string option }

type mode =
  { nickname : Nickname.t
  ; modes : User_mode.modes }

type names =
  { channel : Channel.t
  ; kind : [ `Secret | `Private | `Public ]
  ; names : ([ `Operator | `Voice | `None ] * Nickname.t) list }

type prefix =
  | Server of [ `raw ] Domain_name.t
  | User of { name : Nickname.t
            ; user : string option
            ; host : [ `raw ] Domain_name.t option }

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
  | Mode : mode t
  | Privmsg : (Destination.t list * string) t
  | Ping : [ `raw ] Domain_name.t list t
  | Pong : [ `raw ] Domain_name.t list t
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
  | RPL : reply t

type command = Command : 'a t -> command
type message = Message : 'a t * 'a -> message

let command_of_line (_, command, _) = match String.lowercase_ascii command with
  | "pass" -> Ok (Command Pass)
  | "nick" -> Ok (Command Nick)
  | "user" -> Ok (Command User)
  | "server" -> Ok (Command Server)
  | "oper" -> Ok (Command Oper)
  | "quit" -> Ok (Command Quit)
  | "squit" -> Ok (Command SQuit)
  | "notice" -> Ok (Command Notice)
  | "join" -> Ok (Command Join)
  | "mode" -> Ok (Command Mode)
  | "privmsg" -> Ok (Command Privmsg)
  | "ping" -> Ok (Command Ping)
  | "pong" -> Ok (Command Pong)
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
  | _ -> Rresult.R.error_msgf "Unknown command: %S" command

type send = Send : 'a t * 'a -> send

type 'a recv =
  | Recv : 'a t -> 'a recv
  | Any : message recv

let to_prefix : prefix option -> _ = function
  | Some (User { name; user; host; }) -> Some (`User (Nickname.to_string name, user, Option.map Domain_name.to_string host))
  | Some (Server v) -> Some (`Server (Domain_name.to_string v))
  | None -> None

let of_prettier pp = function
  | `Pretty v -> Some (Fmt.strf "%a" pp v)
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
    | User, { username; hostname; servername; realname; } ->
      let hostname = Domain_name.to_string hostname in
      let servername = Domain_name.to_string servername in
      to_prefix prefix, "user", ([ username; hostname; servername; ], Some realname)
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
    | Notice, { nickname; msg; } ->
      to_prefix prefix, "notice", ([ nickname ], Some msg)
    | Join, channels ->
      let channels, keys = List.split channels in
      let keys, _ = List.partition Option.is_some keys in
      let keys = List.map Option.get keys in
      let channels = List.map Channel.to_string channels in
      let channels = String.concat "," channels in
      let keys = String.concat "," keys in
      to_prefix prefix, "join", ([ channels; keys; ], None)
    | Mode, { nickname; modes; } ->
      to_prefix prefix, "mode", ([ Nickname.to_string nickname ], Some (User_mode.to_string modes))
    | Privmsg, (dsts, msg) ->
      let dsts = List.map Destination.to_string dsts in
      let dsts = String.concat "," dsts in
      to_prefix prefix, "privmsg", ([ dsts ], Some msg)
    | Ping, servers ->
      let servers = List.map Domain_name.to_string servers in
      to_prefix prefix, "ping", (servers, None)
    | Pong, servers ->
      let servers = List.map Domain_name.to_string servers in
      to_prefix prefix, "pong", (servers, None)
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
         -> (prefix option * a, [ `Invalid_parameters | `Invalid_command | `Invalid_reply ]) result
  = fun w ((prefix, command, vs) as line) ->
  let prefix : prefix option = match prefix with
    | None -> None
    | Some (`Server servername) ->
      let servername = Domain_name.of_string_exn servername in
      Some (Server servername)
    | Some (`User (name, user, host)) ->
      let name = Nickname.of_string_exn name in
      let host = Option.map Domain_name.of_string_exn host in
      Some (User { name; user; host; }) in
  match w, String.lowercase_ascii command, vs with
  | Recv Pass, "pass", ([ pass ], _) -> Ok (prefix, pass)
  | Recv Nick, "nick", ([ nick ], _) ->
    ( match Nickname.of_string nick with
    | Ok nick -> Ok (prefix, { nick; hopcount= None; })
    | Error _ -> Error `Invalid_parameters )
  | Recv Nick, "nick", ([ nick; hopcount; ], _) ->
    ( try Ok (prefix, { nick= Nickname.of_string_exn nick; hopcount= Some (int_of_string hopcount) })
      with _ -> Error `Invalid_parameters )
  | Recv User, "user", ([ username; hostname; servername; ], realname) ->
    let realname = Option.value ~default:"" realname in
    ( try let hostname = Domain_name.of_string_exn hostname in
          let servername = Domain_name.of_string_exn servername in
          Ok (prefix, { username; hostname; servername; realname; })
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
  | Recv Notice, "notice", ([ nickname ], Some msg) ->
    Ok (prefix, { nickname; msg; })
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
  | Recv Mode, "mode", ([ nickname ], Some modes) ->
    ( match Nickname.of_string nickname, User_mode.of_string modes with
    | Ok nickname, Ok modes -> Ok (prefix, { nickname; modes; })
    | _ -> Error `Invalid_parameters )
  | Recv Mode, "mode", ([ nickname; modes; ], None) ->
    ( match Nickname.of_string nickname, User_mode.of_string modes with
    | Ok nickname, Ok modes -> Ok (prefix, { nickname; modes; })
    | _ -> Error `Invalid_parameters )
  | Recv Privmsg, "privmsg", ([ dsts ], msg) ->
    ( try let dsts = Destination.of_string_exn dsts in
          let msg = Option.value ~default:"" msg in
          Ok (prefix, (dsts, msg))
      with _ -> Error `Invalid_parameters )
  | Recv Ping, "ping", (servers, server) ->
    let servers = match server with Some v -> servers @ [ v ] | None -> servers in
    ( try let servers = List.map Domain_name.of_string_exn servers in
          Ok (prefix, servers)
      with _ -> Error `Invalid_parameters )
  | Recv Pong, "pong", (servers, server) ->
    let servers = match server with Some v -> servers @ [ v ] | None -> servers in
    ( try let servers = List.map Domain_name.of_string_exn servers in
          Ok (prefix, servers)
      with _ -> Error `Invalid_parameters )
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
  | Any, _, _ ->
    ( match command_of_line line with
    | Error _ -> Error `Invalid_command
    | Ok (Command c) -> match of_line (Recv c) line with
      | Ok (prefix, v) -> Ok (prefix, Message (c, v))
      | Error _ as err -> err )
  | _ -> Error `Invalid_command

let prefix ?user ?host name : prefix =
  User { name; user; host; }

let send : type a. a t -> a -> send
  = fun w v -> Send (w, v)
let recv : type a. a t -> a recv
  = fun w -> Recv w
let any = Any

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
  : type a. Decoder.decoder -> a recv -> (prefix option * a, [> error ]) Decoder.state
  = fun decoder w ->
    let k line decoder = match of_line w line with
      | Ok v -> Decoder.junk_eol decoder ; Decoder.Done v
      | Error err -> Decoder.leave_with decoder (err :> error) in
    Decoder.peek_line ~k decoder
    |> Decoder.reword_error (fun err -> (err :> error))
