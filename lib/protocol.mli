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
type send = Send : 'a t * 'a -> send

type 'a recv =
  | Recv : 'a t -> 'a recv
  | Any : message recv

type prefix =
  { name : string
  ; user : string option
  ; host : [ `raw ] Domain_name.t option }

val encode : ?prefix:prefix -> Encoder.encoder -> send -> [> Encoder.error ] Encoder.state

type error =
  [ Decoder.error
  | `Invalid_command
  | `Invalid_parameters ]

val pp_error : error Fmt.t

val decode : Decoder.decoder -> 'a recv -> (prefix option * 'a, error) Decoder.state
