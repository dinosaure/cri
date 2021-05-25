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

type 'a t =
  | Pass : string t
  | Nick : nick t
  | User : user t
  | Server : server t
  | Oper : oper t
  | Quit : string t
  | SQuit : ([ `raw ] Domain_name.t * string) t
  | Notice : notice t

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
  | _ -> Rresult.R.error_msgf "Unknown command: %S" command

type send = Send : 'a t * 'a -> send

type 'a recv =
  | Recv : 'a t -> 'a recv
  | Any : message recv

type prefix =
  { name : string
  ; user : string option
  ; host : [ `raw ] Domain_name.t option }

let to_prefix = function
  | Some { name; user; host; } -> Some (name, user, Option.map Domain_name.to_string host)
  | None -> None

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
  | Any, _, _ ->
    ( match command_of_line line with
    | Error _ -> Error `Invalid_command
    | Ok (Command c) -> match of_line (Recv c) line with
      | Ok (prefix, v) -> Ok (prefix, Message (c, v))
      | Error _ as err -> err )
  | _ -> Error `Invalid_command

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
