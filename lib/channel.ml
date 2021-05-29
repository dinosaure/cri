type t = string

let for_all p str =
  let res = ref true in
  for i = 0 to String.length str - 1
  do res := !res && p str.[i] done ; !res

module BNF = struct
  open Angstrom

  let chanstring = take_while1 @@ function
    | '\x01' .. '\x07'
    | '\x08' .. '\x09'
    | '\x0b' .. '\x0c'
    | '\x0e' .. '\x1f'
    | '\x21' .. '\x2b'
    | '\x2d' .. '\x39'
    | '\x3b' .. '\xff' -> true
    | _ -> false
  (* XXX(dinosaure): according to RFC 2812, [chanstring] is only
   * one byte. Here, we use [take_while1] - so one or many bytes.
   * I'm not sure that is the right way. *)

  let channelid = take 5 >>= fun str ->
    if for_all (function 'A' .. 'Z' | '0' .. '9' -> true | _ -> false) str
    then return str else fail "channelid"

  let channel = peek_char >>= function
    | Some ('#' | '+' | '&') ->
      chanstring >>= fun a -> option "" (char ':' *> chanstring >>| ( ^ ) ":") >>= fun b ->
      return (a ^ b)
    | Some '!' ->
      (advance 1 *> channelid >>| ( ^ ) "!") >>= fun a ->
      chanstring >>= fun b ->
      option "" (char ':' *> chanstring >>| ( ^ ) ":") >>= fun c ->
      return (a ^ b ^ c)
    | _ ->
      chanstring >>= fun a ->
      option "" (char ':' *> chanstring >>| ( ^ ) ":") >>= fun b ->
      return (a ^ b)

  let validate str =
    match Angstrom.parse_string ~consume:All channel str with
    | Ok _ -> true | Error _ -> false
end

let of_string str =
  if BNF.validate str
  then Ok str
  else Rresult.R.error_msgf "Invalid channel: %S" str

let of_string_exn str =
  match of_string str with
  | Ok v -> v
  | Error (`Msg err) -> invalid_arg err

let to_string x = x

let pp = Fmt.string
let equal = String.equal
