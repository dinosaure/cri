type error = [ `End_of_input | `Expected_eol | `Expected_line | `Invalid_line of string ]

val pp_error : error Fmt.t

type 'err info = { error : 'err; buffer : bytes; committed : int; max : int; }

val pp_error_with_info : pp:'err Fmt.t -> 'err info Fmt.t

type ('v, 'err) state =
  | Done of int * 'v
  | Read of { buffer : Bytes.t
            ; off : int; len : int
            ; continue : int -> ('v, 'err) state }
  | Error of 'err info

type decoder

val decoder : unit -> decoder
val decoder_from : string -> decoder
val io_buffer_size : int

type host =
  [ `Host of string
  | `Ip6 of Ipaddr.V6.t ]

type t =
   [ `User of (string * string option * host option) | `Server of host ] option
  * string * (string list * string option)

module BNF : sig
  val name : string Angstrom.t
  val host : [ `Host of string | `Ip6 of Ipaddr.V6.t ] Angstrom.t
  val servername : [ `Host of string | `Ip6 of Ipaddr.V6.t ] Angstrom.t
  val user : string Angstrom.t
end

val at_least_one_line : decoder -> bool

val junk_eol : decoder -> unit

val peek_line :
     k:(t -> decoder -> ('v, [> error ] as 'err) state)
  -> decoder -> ('v, 'err) state

val leave_with : decoder -> 'err -> (_, 'err) state

val return : decoder -> 'a -> ('a, _) state
val bind : ('a, 'err) state -> ('a -> ('b, 'err) state) -> ('b, 'err) state
val reword_error : ('err0 -> 'err1) -> ('a, 'err0) state -> ('a, 'err1) state
