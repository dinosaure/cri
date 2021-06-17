(* {1 Cri with lwt.}

   An IRC client/server is two concurrent threads:
   - one to read
   - one to write

   They can be synchronized each others by a shared state. [Cri_lwt] provides
   the ground-zero about IRC. It does not describe any high-level logics about
   what the bot (if you want to implement a client) or the server must do.

   It gives you only:
   - a function to wait the next message
   - a function to send the next message
   - a thread which fills and consumes the {i reader}
     and the {i writer}.

   By this way, the API permits to the user to describe and implement what he
   wants without the hard logic about the I/O. Such design is inherent to what
   IRC is: the protocol does not describe a synchronization mechanism between
   the reader and the writer. In others words, if you send a command, you can
   have something else than the expected reply!

   The job to ignore, consume and move forward about a given {i state} (if you
   are connected, authentified, etc.) is at the discretion of the user. From
   such interface, you can implement a monadic-view of the IRC protocol with
   a state containing the {i reader} {b and} the {i writer} - but you will
   loose the concurrent capacity between them. Such scenario can help you if
   you want to implement a bot. However, it does not fit for a server! *)

type error =
  [ `End_of_input        
  | `Decoder of Cri.Protocol.error Cri.Decoder.info
  | `Write of Mimic.write_error
  | `Time_out
  | Cri.Encoder.error
  | Mimic.error ]

val pp_error : error Fmt.t

type recv = unit -> (Cri.Protocol.prefix option * Cri.Protocol.message) option Lwt.t
type send = { send : 'a. ?prefix:Cri.Protocol.prefix -> 'a Cri.Protocol.t -> 'a -> unit } [@@unboxed]
type close = unit -> unit

val run :
     ?stop:Lwt_switch.t
  -> ?timeout:(unit -> unit Lwt.t)
  -> Mimic.ctx
  -> [ `Fiber of (unit, error) result Lwt.t ]
     * recv * send * close
