open Bap_report_common

type t

val create : ?size:int -> string -> t

val update : t -> check -> result -> status -> t

val find_result  : t -> check -> (result * status) list

val checks : t -> check list

val name   : t -> string

val size   : t -> int option

val size_hum :  t -> string option

val with_size : t -> int -> t

val time : t -> check -> float option

val time_hum : t -> check -> string option

val with_time : t -> check -> float -> t

val summary : t -> check -> stat

val merge : t -> t -> t option
