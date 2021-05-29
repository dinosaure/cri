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
type send = Send : 'a t * 'a -> send
type 'a recv =
  | Recv : 'a t -> 'a recv
  | Any : message recv

val prefix : ?user:string -> ?host:[ `raw ] Domain_name.t -> Nickname.t -> prefix
val send : 'a t -> 'a -> send
val recv : 'a t -> 'a recv
val any : message recv

val encode : ?prefix:prefix -> Encoder.encoder -> send -> [> Encoder.error ] Encoder.state

type error =
  [ Decoder.error
  | `Invalid_command
  | `Invalid_parameters
  | `Invalid_reply ]

val pp_error : error Fmt.t

val decode : Decoder.decoder -> 'a recv -> (prefix option * 'a, error) Decoder.state
