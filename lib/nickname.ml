type t = string

module BNF = struct
  open Angstrom

  let nickname = peek_char >>= function
    | Some ('a' .. 'z' | 'A' .. 'Z' | '\x5b' .. '\x60' | '\x7b' .. '\x7d') ->
      ( take_while1 @@ function
      | 'a' .. 'z'
      | 'A' .. 'Z'
      | '0' .. '9'
      | '\x5b' .. '\x60'
      | '\x7b' .. '\x7d'
      | '-' -> true
      | _ -> false )
    | _ -> fail "nickname"
end

let for_all p str =
  let res = ref true in
  for i = 0 to String.length str - 1
  do res := !res && p str.[i] done ; !res

let is = function
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '-'
  | '\x5b' .. '\x60'
  | '\x7b' .. '\x7d' -> true
  | _ -> false

let of_string ?(strict= false) str =
  if str = "" then Rresult.R.error_msgf "A nickname can not be empty"
  else if String.length str > 9 && strict
  then Rresult.R.error_msgf "A nickname can not be larger than 9 bytes"
  else match str.[0] with
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '\x5b' .. '\x60'
    | '\x7b' .. '\x7d' ->
      if for_all is str
      then Ok str else Rresult.R.error_msgf "Invalid nickname: %S" str
    | _ -> Rresult.R.error_msgf "Invalid nickname: %S" str
(* XXX(dinosaure): according to RFC 2812 but be resilient
 * with larger nickname (<3 @kit_ty_kate). *)

let is str = match of_string str with
  | Ok _ -> true | _ -> false

let to_string x = x

let of_string_exn ?strict str = match of_string ?strict str with
  | Ok v -> v | Error (`Msg err) -> invalid_arg err

let pp = Fmt.string
