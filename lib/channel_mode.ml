type t =
  | Channel_creator
  | Channel_operator
  | Voice_privilege
  | Anonymous
  | Invite_only
  | Moderated
  | Ostracize (* n *)
  | Quiet
  | Private
  | Secret
  | Server_reop
  | Topic
  | Key
  | User_limit
  | Ban_mask
  | Exception_mask
  | Invitation_mask

let of_letter = function
  | 'O' -> Ok Channel_creator
  | 'o' -> Ok Channel_operator
  | 'v' -> Ok Voice_privilege
  | 'a' -> Ok Anonymous
  | 'i' -> Ok Invite_only
  | 'm' -> Ok Moderated
  | 'n' -> Ok Ostracize
  | 'q' -> Ok Quiet
  | 'p' -> Ok Private
  | 's' -> Ok Secret
  | 'r' -> Ok Server_reop
  | 't' -> Ok Topic
  | 'k' -> Ok Key
  | 'l' -> Ok User_limit
  | 'b' -> Ok Ban_mask
  | 'e' -> Ok Exception_mask
  | 'I' -> Ok Invitation_mask
  | chr -> Rresult.R.error_msgf "Invalid channel mode letter: %S"
    (String.make 1 chr)

let to_letter = function
  | Channel_creator -> 'O'
  | Channel_operator -> 'o'
  | Voice_privilege -> 'v'
  | Anonymous -> 'a'
  | Invite_only -> 'i'
  | Moderated -> 'm'
  | Ostracize -> 'n'
  | Quiet -> 'q'
  | Private -> 'p'
  | Secret -> 's'
  | Server_reop -> 'r'
  | Topic -> 't'
  | Key -> 'k'
  | User_limit -> 'l'
  | Ban_mask -> 'b'
  | Exception_mask -> 'e'
  | Invitation_mask -> 'I'

type modes =
  { add : t list
  ; rem : t list }

let of_string ?(ignore= true) str =
  let add v lst =
    if List.exists ((=) v) lst
    then lst else v :: lst in
  let rec go ~neg acc idx =
    if idx >= String.length str then Ok acc
    else match str.[idx] with
    | '+' -> go ~neg:false acc (succ idx)
    | '-' -> go ~neg:true  acc (succ idx)
    | chr -> match of_letter chr with
      | Error _ when ignore -> go ~neg acc (succ idx)
      | Error _ -> Rresult.R.error_msgf "Bad modes: %S" str
      | Ok v ->
        let acc =
          if neg then { acc with rem= add v acc.rem }
          else { acc with add= add v acc.add } in
        go ~neg acc (succ idx) in
  go ~neg:false { add= []; rem= []; } 0

let to_string { add; rem; } =
  let len = match add, rem with
    | [], _ :: _ -> 1 + List.length rem
    | _ :: _, [] -> 1 + List.length add
    | [], [] -> 0
    | _ :: _, _ :: _ -> 2 + List.length add + List.length rem in
  let res = Bytes.create len in
  let pos = ref 0 in
  if add <> []
  then ( Bytes.set res !pos '+' ; incr pos
       ; List.iter (fun v -> Bytes.set res !pos (to_letter v) ; incr pos) add ) ;
  if rem <> []
  then ( Bytes.set res !pos '-' ; incr pos
       ; List.iter (fun v -> Bytes.set res !pos (to_letter v) ; incr pos) rem ) ;
  Bytes.unsafe_to_string res

let of_string_exn ?ignore str = match of_string ?ignore str with
  | Ok v -> v | Error (`Msg err) -> invalid_arg err
