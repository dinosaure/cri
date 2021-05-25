[@@@warning "-8"]

type nick = { nick : string; hopcount : int option; }

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

type prefix =
  { name : string
  ; user : string option
  ; host : [ `raw ] Domain_name.t option }

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
  | RPL_WELCOME : welcome prettier t
  | RPL_LUSERCLIENT : discover prettier t
  | RPL_YOURHOST : ([ `raw ] Domain_name.t * string) prettier t
  | RPL_CREATED : Ptime.t prettier t
  | RPL_MYINFO : string option (* TODO *) t
  | RPL_BOUNCE : string option (* TODO *) t

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
  | "001" -> Ok (Command RPL_WELCOME)
  | "002" -> Ok (Command RPL_YOURHOST)
  | "003" -> Ok (Command RPL_CREATED)
  | "004" -> Ok (Command RPL_MYINFO)
  | "005" -> Ok (Command RPL_BOUNCE)
  | "251" -> Ok (Command RPL_LUSERCLIENT)
  | _ -> Rresult.R.error_msgf "Unknown command: %S" command

type send = Send : 'a t * 'a -> send

type 'a recv =
  | Recv : 'a t -> 'a recv
  | Any : message recv

let to_prefix = function
  | Some { name; user; host; } -> Some (name, user, Option.map Domain_name.to_string host)
  | None -> None

let of_prettier pp = function
  | `Pretty v -> Some (Fmt.strf "%a" pp v)
  | `String str -> Some str
  | `None -> None

let to_line
  : type a. ?prefix:prefix -> a t -> a -> Encoder.t
  = fun ?prefix w v -> match w, v with
    | Pass, v -> to_prefix prefix, "pass", ([ v ], None)
    | Nick, { nick; hopcount= None; } ->
      to_prefix prefix, "nick", ([ nick ], None)
    | Nick, { nick; hopcount= Some hopcount; } ->
      to_prefix prefix, "nick", ([ nick; string_of_int hopcount ], None)
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

let apply_keys channels keys =
  let rec go acc channels keys = match channels with
    | [] -> List.rev acc
    | channel :: channels -> match keys with
      | key :: keys -> go ((channel, Some key) :: acc) channels keys
      | [] -> go ((channel, None) :: acc) channels [] in
  go [] channels keys

let rec of_line
  : type a. a recv
         -> Decoder.t
         -> (prefix option * a, [ `Invalid_parameters | `Invalid_command ]) result
  = fun w ((prefix, command, vs) as line) ->
  let prefix = match prefix with
    | None -> None
    | Some (name, user, host) ->
      let host = Option.map Domain_name.of_string_exn host in
      Some { name; user; host; } in
  match w, String.lowercase_ascii command, vs with
  | Recv Pass, "pass", ([ pass ], _) -> Ok (prefix, pass)
  | Recv Nick, "nick", ([ nick ], _) -> Ok (prefix, { nick; hopcount= None; })
  | Recv Nick, "nick", ([ nick; hopcount; ], _) ->
    ( try Ok (prefix, { nick; hopcount= Some (int_of_string hopcount) })
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
  | Recv RPL_WELCOME, "001", (_, Some msg) -> Ok (prefix, `String msg)
  | Recv RPL_WELCOME, "001", (_, None) -> Ok (prefix, `None)
  | Recv RPL_YOURHOST, "002", (_, Some msg) -> Ok (prefix, `String msg)
  | Recv RPL_YOURHOST, "002", (_, None) -> Ok (prefix, `None)
  | Recv RPL_CREATED, "003", (_, Some msg) -> Ok (prefix, `String msg)
  | Recv RPL_CREATED, "003", (_, None) -> Ok (prefix, `None)
  | Recv RPL_MYINFO, "004", (_, msg) -> Ok (prefix, msg)
  | Recv RPL_BOUNCE, "005", (_, msg) -> Ok (prefix, msg)
  | Recv RPL_LUSERCLIENT, "251", (_, Some msg) -> Ok (prefix, `String msg)
  | Recv RPL_LUSERCLIENT, "251", (_, None) -> Ok (prefix, `None)
  | Any, _, _ ->
    ( match command_of_line line with
    | Error _ -> Error `Invalid_command
    | Ok (Command c) -> match of_line (Recv c) line with
      | Ok (prefix, v) -> Ok (prefix, Message (c, v))
      | Error _ as err -> err )
  | _ -> Error `Invalid_command

let prefix ?user ?host name =
  { name; user; host; }

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

type error = [ `Invalid_command | `Invalid_parameters | Decoder.error ]

let pp_error ppf = function
  | `Invalid_command -> Fmt.string ppf "Invalid command"
  | `Invalid_parameters -> Fmt.string ppf "Invalid parameters"
  | #Decoder.error as err -> Decoder.pp_error ppf err

let decode
  : type a. Decoder.decoder -> a recv -> (prefix option * a, [> error ]) Decoder.state
  = fun decoder w ->
    let k line decoder = match of_line w line with
      | Ok v -> Decoder.junk_eol decoder ; Decoder.Done v
      | Error err -> Decoder.leave_with decoder (err :> error) in
    Decoder.peek_line ~k decoder
    |> Decoder.reword_error (fun err -> (err :> error))
