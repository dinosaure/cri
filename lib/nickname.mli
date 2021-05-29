type t

val of_string : ?strict:bool -> string -> (t, [> `Msg of string ]) result
val of_string_exn : ?strict:bool -> string -> t
val to_string : t -> string
val pp : t Fmt.t
