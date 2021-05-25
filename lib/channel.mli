type t

val of_string : string -> (t, [> `Msg of string ]) result
val of_string_exn : string -> t
val to_string : t -> string
