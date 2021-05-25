type decoder = { buffer : bytes; mutable pos : int; mutable max : int }

let io_buffer_size = 65536

let decoder () = { buffer= Bytes.create io_buffer_size; pos= 0; max= 0; }

let decoder_from x =
  let max = String.length x in
  let buffer = Bytes.of_string x in
  { buffer; pos= 0; max }

type error = [ `End_of_input | `Expected_eol | `Expected_line | `Invalid_line of string ]

let pp_error ppf = function
  | `End_of_input -> Fmt.string ppf "End of input"
  | `Expected_eol -> Fmt.string ppf "Expected End-Of-Line"
  | `Expected_line -> Fmt.string ppf "Expected a line"
  | `Invalid_line line -> Fmt.pf ppf "Invalid line: %S" line

type 'err info = { error : 'err; buffer : bytes; committed : int; max : int; }

let pp_error_with_info ~pp ppf { error; buffer; committed; max; } =
  let str = Bytes.sub_string buffer committed (max - committed) in
  Fmt.pf ppf "Decoding error: %a: @[<hov>%a@]"
    pp error (Hxd_string.pp Hxd.default) str

exception Leave of error info

let leave_with (decoder : decoder) error =
  raise (Leave { error; buffer= decoder.buffer; committed= decoder.pos; max= decoder.max; })

type ('v, 'err) state =
  | Done of 'v
  | Read of { buffer : Bytes.t
            ; off : int; len : int
            ; continue : int -> ('v, 'err) state }
  | Error of 'err info

let return v = Done v

let at_least_one_line decoder =
  let pos = ref decoder.pos in
  let chr = ref '\000' in
  let has_cr = ref false in
  while !pos < decoder.max && ( chr := Bytes.unsafe_get decoder.buffer !pos ;
        not (!chr = '\n' && !has_cr) )
  do has_cr := !chr = '\r' ; incr pos done ;
  !pos < decoder.max && !chr = '\n' && !has_cr

let safe :
  (decoder -> ('v, [> error ] as 'err) state) -> decoder -> ('v, 'err) state
  = fun k decoder ->
    try k decoder with Leave { error= #error as error; buffer; committed; max; } ->
      Error { error= (error :> 'err); buffer; committed; max; }

let rec prompt
  :  k:(decoder -> ('v, ([> error ] as 'err)) state)
  -> decoder -> ('v, 'err) state
  = fun ~k decoder ->
    if decoder.pos > 0
    then ( let rest = decoder.max - decoder.pos in
           Bytes.unsafe_blit decoder.buffer decoder.pos decoder.buffer 0 rest ;
           decoder.max <- rest ;
           decoder.pos <- 0 ) ;
    go ~k decoder decoder.max
and go ~k decoder off =
  if off = Bytes.length decoder.buffer
  then assert false
  else if not (at_least_one_line { decoder with max = off })
  then Read { buffer= decoder.buffer; off; len= Bytes.length decoder.buffer - off;
              continue= (fun len -> go ~k decoder (off + len)); }
  else ( decoder.max <- off ; safe k decoder )

let peek_while_eol decoder =
  let idx = ref decoder.pos in
  let chr = ref '\000' in
  let has_cr = ref false in
  while !idx < decoder.max && ( chr := Bytes.unsafe_get decoder.buffer !idx ;
        not (!chr = '\n' && !has_cr) )
  do has_cr := !chr == '\r' ; incr idx done ;
  if !idx < decoder.max && !chr = '\n' && !has_cr
  then (decoder.buffer, decoder.pos, !idx + 1 - decoder.pos)
  else leave_with decoder `Expected_eol

module BNF = struct
  open Angstrom

  let name = peek_char >>= function
    | None | Some ('a' .. 'z' | 'A' .. 'Z') ->
      ( take_while1 @@ function
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' -> true
      | _ -> false ) >>= fun str ->
      if str.[String.length str - 1] = '-'
      then fail "name"
      else return str
    | _ -> fail "name"

  let host = name >>= fun x -> many (char '.' *> name) >>= fun r -> return (x :: r)
  let host = host >>| String.concat "."
  let servername = host

  let is_letter = function
    | 'a' .. 'z' | 'A' .. 'Z' -> true
    | _ -> false

  let is_number = function
    | '0' .. '9' -> true
    | _ -> false

  let is_special = function
    | '-' | '[' | ']' | '\\' | '`' | '^' | '{' | '}' -> true
    | _ -> false

  let is_space = (=) ' '

  let is_non_white = function
    | '\x20' | '\x00' | '\x0d' | '\x0a' -> false
    | _ -> true

  let user = take_while1 is_non_white

  let ( || ) f g = fun chr -> f chr || g chr

  let nick =
    satisfy is_letter >>= fun chr ->
    take_while (is_letter || is_number || is_special) >>= fun str ->
    return (String.make 1 chr ^ str)

  let prefix =
    (servername <|> nick) >>= fun who ->
    option None (char '!' *> user >>| Option.some) >>= fun user ->
    option None (char '@' *> host >>| Option.some) >>= fun host ->
    return (who, user, host) <* take_while1 is_space

  let for_all p str =
    let res = ref true in
    String.iter (fun chr -> res := !res && p chr) str ; !res

  let trailing = take_while @@ function
    | '\x00' | '\r' | '\n' -> false
    | _ -> true

  let failf fmt = Fmt.kstrf fail fmt

  let command =
        (take_while1 is_letter)
    <|> (take 3 >>= fun str -> if for_all is_number str then return str else failf "command %S" str)

  let middle =
    peek_char >>= function
    | None | Some ':' -> fail "middle"
    | _ -> take_while1 @@ function
      | '\x00' | ' ' | '\r' | '\n' -> false
      | _ -> true

  let params =
    fix @@ fun params ->
    take_while1 is_space *>
      option ([], None)
        (    (char ':' *> trailing >>| fun v -> [], Some v)
        <|> (middle >>= fun x -> params >>= fun (r, trailing) -> return ((x :: r), trailing)) )

  let crlf = string "\r\n"

  let message =
    option None ((char ':' *> prefix) >>| Option.some <?> "prefix") >>= fun prefix ->
    (command <?> "command") >>= fun command ->
    (params <?> "params") >>= fun params ->
    return (prefix, command, params) <* crlf
end

let junk_eol decoder =
  let idx = ref decoder.pos in
  let chr = ref '\000' in
  let has_cr = ref false in
  while !idx < decoder.max && ( chr := Bytes.unsafe_get decoder.buffer !idx ;
        not (!chr = '\n' && !has_cr) )
  do has_cr := !chr == '\r' ; incr idx done ;
  if !idx < decoder.max && !chr = '\n' && !has_cr
  then decoder.pos <- !idx + 1
  else leave_with decoder `Expected_eol

type t = (string * string option * string option) option * string * (string list * string option)

let peek_line ~k decoder =
  let k decoder =
    let (buffer, off, len) = peek_while_eol decoder in
    let str = Bytes.sub_string buffer off len in
    match Angstrom.parse_string ~consume:All BNF.message str with
    | Ok v -> k v decoder
    | Error _ ->
      let line = String.sub str 0 (String.length str - 2) in
      decoder.pos <- decoder.pos + len ;
      leave_with decoder (`Invalid_line line) in
  prompt ~k decoder

let leave_with (decoder : decoder) error =
  Error { error; buffer= decoder.buffer; committed= decoder.pos; max= decoder.max; }

let rec bind x f = match x with
  | Done v -> f v
  | Read { buffer; off; len; continue; } ->
    let continue len = bind (continue len) f in
    Read { buffer; off; len; continue; }
  | Error err -> Error err

let rec reword_error f = function
  | Done v -> Done v
  | Read { buffer; off; len; continue; } ->
    let continue len = reword_error f (continue len) in
    Read { buffer; off; len; continue; }
  | Error ({ error; _ } as info) -> Error { info with error = (f error) }
