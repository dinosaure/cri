type error =
  [ `End_of_input        
  | `Decoder of Cri.Protocol.error Cri.Decoder.info
  | `Write of Mimic.write_error
  | Cri.Encoder.error
  | Mimic.error ]

val pp_error : error Fmt.t

type recv = unit -> (Cri.Protocol.prefix option * Cri.Protocol.message) option Lwt.t
type send = { send : 'a. ?prefix:Cri.Protocol.prefix -> 'a Cri.Protocol.t -> 'a -> unit } [@@unboxed]
type close = unit -> unit

val run :
     ?stop:Lwt_switch.t
  -> ctx:Mimic.ctx
  -> [ `Fiber of (unit, error) result Lwt.t ]
     * recv * send * close
