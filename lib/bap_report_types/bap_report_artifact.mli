open Bap_report_common_types

type t

type result = string list
[@@deriving compare, sexp]

type kind =
  | Physical
  | Virtual

val create : ?size:int -> ?kind:kind -> string -> t

val update : t -> check -> result -> status -> t

val find_check  : t -> check -> (result * status) list
val find_status : t -> check -> result -> status option

val checks : t -> (check * (result * status) list) list

val name   : t -> string

val kind : t -> kind option

val size   : t -> int option
val size_hum :  t -> string option
val with_size : t -> int -> t

val time : t -> check -> float option
val time_hum : t -> check -> string option
val with_time : t -> check -> float -> t

val summary : t -> check -> stat
val merge : t -> t -> t option
