type error = [ `End_of_input | `Expected_eol | `Expected_line | `Invalid_line of string ]

val pp_error : error Fmt.t

type 'err info = { error : 'err; buffer : bytes; committed : int; max : int; }

val pp_error_with_info : pp:'err Fmt.t -> 'err info Fmt.t

type ('v, 'err) state =
  | Done of 'v
  | Read of { buffer : Bytes.t
            ; off : int; len : int
            ; continue : int -> ('v, 'err) state }
  | Error of 'err info

type decoder

val decoder : unit -> decoder
val decoder_from : string -> decoder
val io_buffer_size : int

type t =
   [ `User of (string * string option * string option) | `Server of string ] option
  * string * (string list * string option)

val junk_eol : decoder -> unit

val peek_line :
     k:(t -> decoder -> ('v, [> error ] as 'err) state)
  -> decoder -> ('v, 'err) state

val leave_with : decoder -> 'err -> (_, 'err) state

val return : 'a -> ('a, _) state
val bind : ('a, 'err) state -> ('a -> ('b, 'err) state) -> ('b, 'err) state
val reword_error : ('err0 -> 'err1) -> ('a, 'err0) state -> ('a, 'err1) state
