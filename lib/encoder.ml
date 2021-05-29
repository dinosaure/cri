type encoder = { payload : bytes; mutable pos : int; }

type error = [ `No_enough_space ]

let pp_error ppf = function
  | `No_enough_space -> Fmt.string ppf "No enough space"

type 'err state =
  | Write of { buffer : string; off : int; len : int; continue : int -> 'err state; }
  | Error of 'err
  | Done

let io_buffer_size = 65536

let encoder () = { payload= Bytes.create io_buffer_size; pos= 0 }

exception Leave of error

let leave_with (_ : encoder) error = raise (Leave error)

let safe
  : (unit -> ([> error ] as 'err) state) -> 'err state
  = fun k ->
    try k () with Leave (#error as err) -> Error (err : 'err)

let flush k0 encoder =
  if encoder.pos > 0
  then
    let rec k1 n = if n < encoder.pos
      then Write { buffer= Bytes.unsafe_to_string encoder.payload;
                   off= n; len= encoder.pos - n; continue= (fun m -> k1 (n + m)) }
      else ( encoder.pos <- 0 ; safe k0 ) in
    k1 0
  else safe k0

let write str encoder =
  let max = Bytes.length encoder.payload in
  let go j l encoder =
    let rem = max - encoder.pos in
    let len = if l > rem then rem else l in
    Bytes.blit_string str j encoder.payload encoder.pos len ;
    encoder.pos <- encoder.pos + len ;
    if len < l then leave_with encoder `No_enough_space in
  go 0 (String.length str) encoder

let blit ~buf ~off ~len encoder =
  let max = Bytes.length encoder.payload in
  let go j l encoder =
    let rem = max - encoder.pos in
    let len = if l > rem then rem else l in
    Bytes.blit_string buf (off + j) encoder.payload encoder.pos len ;
    encoder.pos <- encoder.pos + len ;
    if len < l then leave_with encoder `No_enough_space in
  go 0 len encoder

type t =
    [ `User of (string * string option * string option) | `Server of string ] option
  * string * (string list * string option)

let write_crlf encoder = write "\r\n" encoder
let write_space encoder = write " " encoder

let write_prefix prefix encoder = match prefix with
  | `User (name, None, None) ->
    write ":" encoder ;
    write name encoder ;
    write_space encoder
  | `User (name, Some user, None) ->
    write ":" encoder ;
    write name encoder ;
    write "!" encoder ;
    write user encoder ;
    write_space encoder
  | `User (name, Some user, Some host) ->
    write ":" encoder ;
    write name encoder ;
    write "!" encoder ;
    write user encoder ;
    write "@" encoder ;
    write host encoder ;
    write_space encoder
  | `User (name, None, Some host) ->
    write ":" encoder ;
    write name encoder ;
    write "@" encoder ;
    write host encoder ;
    write_space encoder
  | `Server servername ->
    write ":" encoder ;
    write servername encoder ;
    write_space encoder

let encode_line encoder = function
  | prefix, command, ([], None) ->
    Option.iter (fun prefix -> write_prefix prefix encoder) prefix ;
    write command encoder ;
    write_crlf encoder
  | prefix, command, (params, None) ->
    Option.iter (fun prefix -> write_prefix prefix encoder) prefix ;
    write command encoder ;
    List.iter (fun p -> write_space encoder ; write p encoder) params ;
    write_crlf encoder
  | prefix, command, ([], Some trailing) ->
    Option.iter (fun prefix -> write_prefix prefix encoder) prefix ;
    write command encoder ;
    write_space encoder ;
    write ":" encoder ;
    write trailing encoder ;
    write_crlf encoder
  | prefix, command, (params, Some trailing) ->
    Option.iter (fun prefix -> write_prefix prefix encoder) prefix ;
    write command encoder ;
    List.iter (fun p -> write_space encoder ; write p encoder) params ;
    write_space encoder ;
    write ":" encoder ;
    write trailing encoder ;
    write_crlf encoder

let encode_line k encoder t = encode_line encoder t ; flush k encoder
