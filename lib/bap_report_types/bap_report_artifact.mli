open Bap_report_common

type t

type incident = Bap_report_incident.t

val create : ?size:int -> string -> t

val update : t -> incident -> status -> t

val incidents : ?check:check -> t -> (incident * status) list

val checks : t -> check list

val name   : t -> string
val size   : t -> int option
val size_hum :  t -> string option
val with_size : t -> int -> t
val time : t -> check -> float option
val time_hum : t -> check -> string option
val with_time : t -> check -> float -> t

val summary : t -> check -> stat
