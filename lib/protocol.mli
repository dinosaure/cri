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

type prefix =
  | Server of host
  | User of { name : Nickname.t
            ; user : string option
            ; host : host option }

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

val pp_nick : nick Fmt.t
val pp_user : user Fmt.t
val pp_server : server Fmt.t
val pp_oper : oper Fmt.t
val pp_notice : notice Fmt.t
val pp_prettier : 'a Fmt.t -> 'a prettier Fmt.t
val pp_welcome : welcome Fmt.t
val pp_discover : discover Fmt.t
val pp_reply : reply Fmt.t
val pp_user_mode : user_mode Fmt.t
val pp_names : names Fmt.t
val pp_host : host Fmt.t

val pp_prefix : prefix Fmt.t

type command = Command : 'a t -> command
type message = Message : 'a t * 'a -> message
type send = Send : 'a t * 'a -> send
type 'a recv =
  | Recv : 'a t -> (prefix option * 'a) recv
  | Any : (prefix option * message) recv
  | Many : (prefix option * message) list recv

val pp_message : message Fmt.t

val prefix : ?user:string -> ?host:[ `raw ] Domain_name.t -> Nickname.t -> prefix
val send : 'a t -> 'a -> send
val recv : 'a t -> (prefix option * 'a) recv
val any : (prefix option * message) recv
val many : (prefix option * message) list recv

val encode : ?prefix:prefix -> Encoder.encoder -> send -> [> Encoder.error ] Encoder.state

type error =
  [ Decoder.error
  | `Invalid_command
  | `Invalid_parameters
  | `Invalid_reply ]

val pp_error : error Fmt.t

val decode : Decoder.decoder -> 'a recv -> ('a, error) Decoder.state
