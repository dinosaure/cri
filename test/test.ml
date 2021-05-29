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
  | Cri.Decoder.Done (None, Cri.Protocol.Message (Ping, [ v ])) ->
    Alcotest.(check domain_name) "domain-name" v (Domain_name.of_string_exn "zinc.libera.chat")
  | Cri.Decoder.Done _ -> Alcotest.failf "Unexpected message"
  | _ -> Alcotest.failf "Invalid state of decoding"

let () =
        Alcotest.run "cri" [ "BNF", [ test01; test02 ] ]
