type mask = |

type t =
  | Channel of Channel.t
  | User_with_servername of { user : string
                            ; host : [ `raw ] Domain_name.t option
                            ; servername : [ `raw ] Domain_name.t }
  | User_with_host of { user : string
                      ; host : [ `raw ] Domain_name.t }
  | Nickname of Nickname.t
  | Full_nickname of { nick : Nickname.t
                     ; user : string
                     ; host : [ `raw ] Domain_name.t }
  | Mask of mask

val pp : t Fmt.t

module BNF : sig
  val destination : t list Angstrom.t
end

val to_string : t -> string
val of_string : string -> (t list, [> `Msg of string ]) result
val of_string_exn : string -> t list
