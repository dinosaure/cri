type t = Away | Invisible | Wallops | Restricted | Operator | Local_operator | Notices

let of_letter = function
  | 'a' -> Ok Away
  | 'i' -> Ok Invisible
  | 'w' -> Ok Wallops
  | 'r' -> Ok Restricted
  | 'o' -> Ok Operator
  | 'O' -> Ok Local_operator
  | 's' -> Ok Notices
  | chr -> Rresult.R.error_msgf "Invalid user mode letter: %S" (String.make 1 chr)

let to_letter = function
  | Away -> 'a'
  | Invisible -> 'i'
  | Wallops -> 'w'
  | Restricted -> 'r'
  | Operator -> 'o'
  | Local_operator -> 'O'
  | Notices -> 's'
