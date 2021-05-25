type error =
  [ `End_of_input        
  | `Decoder of Cri.Protocol.error Cri.Decoder.info
  | `Write of Mimic.write_error
  | Cri.Encoder.error
  | Mimic.error ]

val pp_error : error Fmt.t

val run :
     ?stop:Lwt_switch.t
  -> ctx:Mimic.ctx
  -> [ `Fiber of (unit, error) result Lwt.t ]
     * (unit -> (Cri.Protocol.prefix option * Cri.Protocol.message) option Lwt.t)
     * ((Cri.Protocol.prefix option * Cri.Protocol.send) option -> unit)
