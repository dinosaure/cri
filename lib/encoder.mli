type error = [ `No_enough_space ]

val pp_error : error Fmt.t

type encoder

type 'err state =
  | Write of { buffer : string; off : int; len : int; continue : int -> 'err state; }
  | Error of 'err
  | Done

val encoder : unit -> encoder
val flush : (unit -> ([> error ] as 'err) state) -> encoder -> 'err state
val write : string -> encoder -> unit
val blit : buf:string -> off:int -> len:int -> encoder -> unit

type t = (string * string option * string option) option * string * (string list * string option)

val encode_line : (unit -> ([> error ] as 'err) state) -> encoder -> t -> 'err state
