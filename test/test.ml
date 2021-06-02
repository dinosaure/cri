let channel = Alcotest.testable Cri.Channel.pp Cri.Channel.equal
let domain_name = Alcotest.testable Domain_name.pp Domain_name.equal

let test01 =
  Alcotest.test_case "destination" `Quick @@ fun () ->
  match Cri.Destination.of_string "#mirage" with
  | Ok [ Channel ch ] ->
    Alcotest.(check channel) "channel" ch (Cri.Channel.of_string_exn "#mirage")
  | Ok vs -> Alcotest.failf "Unexpected destination: %a" Fmt.(Dump.list Cri.Destination.pp) vs
  | Error _ -> Alcotest.failf "Invalid destination: %S" "#mirage"

let test02 =
  Alcotest.test_case "ping" `Quick @@ fun () ->
  let line = "PING :zinc.libera.chat\r\n" in
  let dec = Cri.Decoder.decoder_from line in
  match Cri.Protocol.decode dec Cri.Protocol.any with
  | Cri.Decoder.Done (None, Cri.Protocol.Message (Ping, (None, Some v))) ->
    Alcotest.(check domain_name) "domain-name" v (Domain_name.of_string_exn "zinc.libera.chat")
  | Cri.Decoder.Done _ -> Alcotest.failf "Unexpected message"
  | _ -> Alcotest.failf "Invalid state of decoding"

let test03 =
  Alcotest.test_case "ipv6" `Quick @@ fun () ->
  let line = ":d_bot!~d_bot@2001:4802:7800:1:be76:4eff:fe20:3027 PRIVMSG #ocaml :<Ulugbek> Hi\r\n" in
  let dec = Cri.Decoder.decoder_from line in
  match Cri.Protocol.decode dec Cri.Protocol.any with
  | Cri.Decoder.Done (Some _prefix, Cri.Protocol.Message (Privmsg, ([ Cri.Destination.Channel ch ], msg))) ->
    Alcotest.(check channel) "channel" ch (Cri.Channel.of_string_exn "#ocaml") ;
    Alcotest.(check string) "message" msg "<Ulugbek> Hi"
  | Cri.Decoder.Done _ -> Alcotest.failf "Unexpected message"
  | _ -> Alcotest.failf "Invalid state of decoding"

let test04 =
  Alcotest.test_case "join" `Quick @@ fun () ->
  let line = ":habnabit_!~habnabit@python/site-packages/habnabit JOIN #ocaml\r\n" in
  let dec = Cri.Decoder.decoder_from line in
  match Cri.Protocol.decode dec Cri.Protocol.any with
  | Cri.Decoder.Done (Some _prefix, Cri.Protocol.Message (Join, [ ch, _ ])) ->
    Alcotest.(check channel) "channel" ch (Cri.Channel.of_string_exn "#ocaml")
  | Cri.Decoder.Done _ -> Alcotest.failf "Unexpected message"
  | _ -> Alcotest.failf "Invalid state of decoding"

let test05 =
  Alcotest.test_case "quit" `Quick @@ fun () ->
  let line = ":_whitelogger!~whitelogg@uruz.whitequark.org QUIT :Remote host closed the connection\r\n" in
  let dec = Cri.Decoder.decoder_from line in
  match Cri.Protocol.decode dec Cri.Protocol.Any with
  | Cri.Decoder.Done _ -> ()
  | _ -> Alcotest.failf "Invalid state of decoding"

let () =
        Alcotest.run "cri" [ "BNF", [ test01; test02; test03; test04; test05 ] ]
