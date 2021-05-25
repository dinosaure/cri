type t = string

let of_string str =
  if str = ""
  then Rresult.R.error_msgf "Invalid channel: %S" str
  else match str.[0] with
  | '#' | '&' ->
    let res = ref false in
    for i = 1 to String.length str - 1
    do match str.[i] with
       | ' ' | '\x00' | '\r' | '\n' | ',' | '\b' -> res := true
       | _ -> () done ;
    if !res then Rresult.R.error_msgf "Invalid channel: %S" str
    else Ok str
  | _ -> Rresult.R.error_msgf "Invalid channel: %S" str

let of_string_exn str =
  match of_string str with
  | Ok v -> v
  | Error (`Msg err) -> invalid_arg err

let to_string x = x
