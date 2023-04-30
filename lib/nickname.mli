type t

module BNF : sig
  val nickname : t Angstrom.t
end

val of_string : ?strict:bool -> string -> (t, [> `Msg of string ]) result
val of_string_exn : ?strict:bool -> string -> t
val to_string : t -> string
val pp : t Fmt.t
val is : string -> bool
val equal : t -> t -> bool

module Set : Set.S with type elt = t
