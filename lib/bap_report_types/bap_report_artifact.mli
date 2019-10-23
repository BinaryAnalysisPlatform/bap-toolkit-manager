open Bap_report_common

type t [@@deriving bin_io, compare, sexp]

type incident = Bap_report_incident.t
type incident_kind = Bap_report_incident.kind
type incident_id = Bap_report_incident.id

val create : ?size:int -> string -> t

val update : t -> incident -> status -> t

val incidents : ?kind:incident_kind -> t -> (incident * status) list

val checks : t -> incident_kind list

val name   : t -> string
val size   : t -> int option
val size_hum :  t -> string option
val with_size : t -> int -> t
val time : t -> incident_kind -> float option
val time_hum : t -> incident_kind -> string option
val with_time : t -> incident_kind -> float -> t

val summary : t -> incident_kind -> stat

val find : t -> incident_id -> (incident * status) option

val merge : t -> t -> t option


val no_incidents: t -> incident_kind -> t
