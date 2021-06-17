type t

module BNF : sig
  val channel : t Angstrom.t
end

val of_string : string -> (t, [> `Msg of string ]) result
val of_string_exn : string -> t
val to_string : t -> string
val pp : t Fmt.t
val equal : t -> t -> bool
val is : string -> bool
