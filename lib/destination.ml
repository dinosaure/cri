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

let pp ppf = function
  | Channel v -> Channel.pp ppf v
  | User_with_servername { user; host; servername; } ->
    Fmt.pf ppf "%s%a@%a" user Fmt.(option (prefix (const string "%") Domain_name.pp)) host
      Domain_name.pp servername
  | User_with_host { user; host; } ->
    Fmt.pf ppf "%s%%%a" user Domain_name.pp host
  | Nickname v -> Nickname.pp ppf v
  | Full_nickname { nick; user; host; } ->
    Fmt.pf ppf "%a!%s@%a" Nickname.pp nick
      user Domain_name.pp host
  | Mask _ -> .

module BNF = struct
  open Angstrom

  let channel = Channel.BNF.channel
  let nickname = Nickname.BNF.nickname
  let user = Decoder.BNF.user
  let host = Decoder.BNF.host
  let servername = Decoder.BNF.servername

  let msgto =
        (channel >>| fun v -> Channel v)
    <|> (user >>= fun user -> option None (char '%' *> host >>| Option.some) >>= fun host ->
         char '@' *> servername >>= fun servername ->
         return (User_with_servername { user; host= Option.map Domain_name.of_string_exn host;
                                        servername= Domain_name.of_string_exn servername; }))
    <|> (user >>= fun user -> char '%' *> host >>= fun host ->
         return (User_with_host { user; host= Domain_name.of_string_exn host; }))
    <|> (nickname >>| fun v -> Nickname v)
    <|> (nickname >>= fun nick -> char '!' *> user >>= fun user ->
         char '@' *> host >>= fun host ->
         return (Full_nickname { nick; user; host= Domain_name.of_string_exn host; }))

  let destination = msgto >>= fun x -> many (char ',' *> msgto) >>= fun r -> return (x :: r)
  let crlf = string "\r\n"
end

let to_string = function
  | Channel v -> Channel.to_string v
  | User_with_servername { user; host; servername; } ->
    Fmt.str "%s%a@%a" user Fmt.(option (prefix (const string "%") Domain_name.pp)) host
      Domain_name.pp servername
  | User_with_host { user; host; } ->
    Fmt.str "%s%%%a" user Domain_name.pp host
  | Nickname v -> Nickname.to_string v
  | Full_nickname { nick; user; host; } ->
    Fmt.str "%a!%s@%a" Nickname.pp nick user Domain_name.pp host
  | Mask _ -> .

let of_string str =
  match Angstrom.parse_string ~consume:All
          Angstrom.(BNF.destination <* BNF.crlf) (str ^ "\r\n") with
  | Ok vs -> Ok vs
  | Error _ -> Rresult.R.error_msgf "Invalid target: %S" str

let of_string_exn str = match of_string str with
  | Ok vs -> vs
  | Error (`Msg err) -> invalid_arg err
