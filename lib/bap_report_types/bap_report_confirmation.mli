open Bap_report_common


type t [@@deriving sexp,compare,bin_io]
type kind [@@deriving sexp,compare,bin_io]

val must : kind
val may  : kind

val create : kind ->
             Bap_report_incident.kind ->
             Bap_report_incident.locations -> t

val locations : t -> Bap_report_incident.locations
val incident_kind : t -> Bap_report_incident.kind
val confirmation : t -> kind

val validate : t -> status option -> status

val is : t -> kind -> bool

val id : t -> Bap_report_incident.id
