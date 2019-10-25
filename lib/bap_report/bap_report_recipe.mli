open Core_kernel
open Bap_report_types

type t
type recipe = t

val create : name:string -> desc:string -> t

val add_parameter : t -> name:string -> value:string -> t

val name : t -> string

val description : t -> string

val to_string : t -> string

val provide : t -> incident_kind -> t
val kinds : t -> incident_kind list
