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

type modes =
  { add : t list
  ; rem : t list }

let pp ppf { add; rem; } = match add, rem with
  | [], [] -> ()
  | [], _ :: _ -> Fmt.pf ppf "-%a" Fmt.(list ~sep:nop (using to_letter char)) rem
  | _ :: _, [] -> Fmt.pf ppf "+%a" Fmt.(list ~sep:nop (using to_letter char)) add
  | _ -> Fmt.pf ppf "+%a-%a"
    Fmt.(list ~sep:nop (using to_letter char)) add
    Fmt.(list ~sep:nop (using to_letter char)) rem

let of_string ?(ignore= true) str =
  let add v lst = if List.exists ((=) v) lst then lst else v :: lst in
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
  if add <> [] then ( Bytes.set res !pos '+' ; incr pos
                    ; List.iter (fun v -> Bytes.set res !pos (to_letter v) ; incr pos) add ) ;
  if rem <> [] then ( Bytes.set res !pos '-' ; incr pos
                    ; List.iter (fun v -> Bytes.set res !pos (to_letter v) ; incr pos) rem ) ;
  Bytes.unsafe_to_string res
