type t = string

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

let of_string str =
  if str = "" then Rresult.R.error_msgf "A nickname can not be empty"
  else if String.length str > 9 then Rresult.R.error_msgf "A nickname can not be larger than 9 bytes"
  else match str.[0] with
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '\x5b' .. '\x60'
    | '\x7b' .. '\x7d' ->
      if for_all is str
      then Ok str else Rresult.R.error_msgf "Invalid nickname: %S" str
    | _ -> Rresult.R.error_msgf "Invalid nickname: %S" str

let to_string x = x

let of_string_exn str = match of_string str with
  | Ok v -> v | Error (`Msg err) -> invalid_arg err
