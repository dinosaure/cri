open Mirage

type mimic = Mimic
let mimic = typ Mimic

let empty =
  let packages = [ package "mimic" ] in
  impl @@ object
    inherit base_configurable
    method ty = mimic
    method module_name = "Mimic"
    method! packages = Key.pure packages
    method name = "mimic_ctx"
    method! connect _ _modname _ = "Lwt.return Mimic.empty"
  end

let mimic_count =
  let v = ref (-1) in
  fun () -> incr v ; !v

let mimic_conf () =
  let packages = [ package "mimic" ] in
  impl @@ object
       inherit base_configurable
       method ty = mimic @-> mimic @-> mimic
       method module_name = "Mimic.Merge"
       method! packages = Key.pure packages
       method name = Fmt.str "merge_ctx%02d" (mimic_count ())
       method! connect _ _modname =
         function
         | [ a; b ] -> Fmt.str "Lwt.return (Mimic.merge %s %s)" a b
         | [ x ] -> Fmt.str "%s.ctx" x
         | _ -> Fmt.str "Lwt.return Mimic.empty"
     end

let merge ctx0 ctx1 = mimic_conf () $ ctx0 $ ctx1

let mimic_tcp_conf =
  let packages = [ package "git-mirage" ~sublibs:[ "tcp" ] ] in
  impl @@ object
       inherit base_configurable
       method ty = stackv4v6 @-> mimic
       method module_name = "Git_mirage_tcp.Make"
       method! packages = Key.pure packages
       method name = "tcp_ctx"
       method! connect _ modname = function
         | [ stack ] ->
           Fmt.str {ocaml|Lwt.return (%s.with_stack %s %s.ctx)|ocaml}
             modname stack modname
         | _ -> assert false
     end

let mimic_tcp_impl stackv4v6 = mimic_tcp_conf $ stackv4v6

let mimic_ssh_conf ~kind ~seed ~auth =
  let seed = Key.abstract seed in
  let auth = Key.abstract auth in
  let packages = [ package "git-mirage" ~sublibs:[ "ssh" ] ] in
  impl @@ object
       inherit base_configurable
       method ty = stackv4v6 @-> mimic @-> mclock @-> mimic
       method! keys = [ seed; auth; ]
       method module_name = "Git_mirage_ssh.Make"
       method! packages = Key.pure packages
       method name = match kind with
         | `Rsa -> "ssh_rsa_ctx"
         | `Ed25519 -> "ssh_ed25519_ctx"
       method! connect _ modname =
         function
         | [ _; tcp_ctx; _ ] ->
             let with_key =
               match kind with
               | `Rsa -> "with_rsa_key"
               | `Ed25519 -> "with_ed25519_key"
             in
             Fmt.str
               {ocaml|let ssh_ctx00 = Mimic.merge %s %s.ctx in
                      let ssh_ctx01 = Option.fold ~none:ssh_ctx00
                        ~some:(fun v -> %s.%s v ssh_ctx00) %a in
                      let ssh_ctx02 = Option.fold ~none:ssh_ctx01
                        ~some:(fun v -> %s.with_authenticator v ssh_ctx01) %a in
                      Lwt.return ssh_ctx02|ocaml}
               tcp_ctx modname
               modname with_key Key.serialize_call seed
               modname Key.serialize_call auth
         | _ -> assert false
     end

let mimic_ssh_impl ~kind ~seed ~auth stackv4v6 mimic_git mclock =
  mimic_ssh_conf ~kind ~seed ~auth
  $ stackv4v6
  $ mimic_git
  $ mclock

let mimic_dns_conf =
  let packages = [ package "git-mirage" ~sublibs:[ "dns" ] ] in
  impl @@ object
       inherit base_configurable
       method ty = random @-> mclock @-> time @-> stackv4v6 @-> mimic @-> mimic
       method module_name = "Git_mirage_dns.Make"
       method! packages = Key.pure packages
       method name = "dns_ctx"
       method! connect _ modname =
         function
         | [ _; _; _; stack; tcp_ctx ] ->
             Fmt.str
               {ocaml|let dns_ctx00 = Mimic.merge %s %s.ctx in
                      let dns_ctx01 = %s.with_dns %s dns_ctx00 in
                      Lwt.return dns_ctx01|ocaml}
               tcp_ctx modname
               modname stack
         | _ -> assert false
     end

let mimic_dns_impl random mclock time stackv4v6 mimic_tcp =
  mimic_dns_conf $ random $ mclock $ time $ stackv4v6 $ mimic_tcp

type paf = Paf
let paf = typ Paf

let paf_conf () =
  let packages = [ package "paf" ~sublibs:[ "mirage" ] ] in
  impl @@ object
    inherit base_configurable
    method ty = time @-> stackv4v6 @-> paf
    method module_name = "Paf_mirage.Make"
    method! packages = Key.pure packages
    method name = "paf"
  end

let paf_impl time stackv4v6 = paf_conf () $ time $ stackv4v6

let mimic_paf_conf () =
  let packages = [ package "git-paf" ] in
  impl @@ object
       inherit base_configurable
       method ty = time @-> pclock @-> stackv4v6 @-> paf @-> mimic @-> mimic
       method module_name = "Git_paf.Make"
       method! packages = Key.pure packages
       method name = "paf_ctx"
       method! connect _ modname = function
         | [ _; _; _; _; tcp_ctx; ] ->
             Fmt.str
               {ocaml|let paf_ctx00 = Mimic.merge %s %s.ctx in
                      Lwt.return paf_ctx00|ocaml}
               tcp_ctx modname
         | _ -> assert false
     end

let mimic_paf_impl time pclock stackv4v6 paf mimic_tcp =
  mimic_paf_conf ()
  $ time
  $ pclock
  $ stackv4v6
  $ paf
  $ mimic_tcp

let mimic_cri_conf () =
  let packages = [ package ~sublibs:[ "mirage" ] "cri" ] in
  impl @@ object
       inherit base_configurable
       method ty = random @-> mclock @-> time @-> stackv4v6 @-> mimic
       method module_name = "Cri_mirage.Make"
       method! packages = Key.pure packages
       method name = "cri_ctx"
       method! connect _ modname = function
         | [ _; _; _; stack; ] ->
           Fmt.str {ocaml|let cri_ctx00 = %s.with_stack %s %s.ctx in
                          let cri_ctx01 = %s.with_dns %s cri_ctx00 in
                          Lwt.return cri_ctx01|ocaml}
             modname stack modname
             modname stack
         | _ -> assert false
     end

let mimic_cri_impl random mclock time stackv4v6 =
  mimic_cri_conf () $ random $ mclock $ time $ stackv4v6

(* / *)

let ssh_seed =
  let doc = Key.Arg.info ~doc:"Seed of the private SSH key." [ "ssh-seed" ] in
  Key.(create "ssh_seed" Arg.(opt (some string) None doc))

let ssh_auth =
  let doc = Key.Arg.info ~doc:"SSH public key of the remote Git endpoint." [ "ssh-auth" ] in
  Key.(create "ssh_auth" Arg.(opt (some string) None doc))

let hostname =
  let doc = Key.Arg.info ~doc:"Hostname of the bot." [ "hostname" ] in
  Key.(create "hostname" Arg.(required string doc))

let nickname =
  let doc = Key.Arg.info ~doc:"Nickname of the bot." [ "nickname" ] in
  Key.(create "nickname" Arg.(opt string "noisy-bot" doc))

let channel =
  let doc = Key.Arg.info ~doc:"IRC channel to log." [ "channel" ] in
  Key.(create "channel" Arg.(required string doc))

let tick =
  let doc = Key.Arg.info ~doc:"How long we log the channel (in seconds)." [ "tick" ] in
  Key.(create "tick" Arg.(opt int 3600 doc))

let remote =
  let doc = Key.Arg.info ~doc:"Git repository to save logs." [ "remote" ] in
  Key.(create "remote" Arg.(required string doc))

let irc =
  let doc = Key.Arg.info ~doc:"IRC server to connect." [ "irc" ] in
  Key.(create "irc" Arg.(required string doc))

let logger =
  foreign "Unikernel.Make"
    ~keys:[ Key.abstract hostname
          ; Key.abstract nickname
          ; Key.abstract channel 
          ; Key.abstract tick
          ; Key.abstract remote
          ; Key.abstract irc ]
    (pclock @-> time @-> stackv4v6 @-> mimic @-> mimic @-> job)

let git ~kind ~seed ~auth stackv4v6 random mclock pclock time paf =
  let mtcp = mimic_tcp_impl stackv4v6 in
  let mdns = mimic_dns_impl random mclock time stackv4v6 mtcp in
  let mssh = mimic_ssh_impl ~kind ~seed ~auth stackv4v6 mtcp mclock in
  let mpaf = mimic_paf_impl time pclock stackv4v6 paf mtcp in
  merge mpaf (merge mssh mdns)

let random = default_random
let stackv4v6 = generic_stackv4v6 default_network
let pclock = default_posix_clock
let mclock = default_monotonic_clock
let time = default_time

let paf = paf_impl time stackv4v6
let git = git ~kind:`Rsa ~seed:ssh_seed ~auth:ssh_auth
let git = git stackv4v6 random mclock pclock time paf

let irc = mimic_cri_impl random mclock time stackv4v6

let () =
  register "logger"
    ~packages:[ package "cri" ~sublibs:[ "lwt"; "logger" ]
              ; package "irmin-mirage-git" ]
    [ logger $ pclock $ time $ stackv4v6 $ irc $ git ]
