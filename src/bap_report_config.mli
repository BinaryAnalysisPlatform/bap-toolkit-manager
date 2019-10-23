open Bap_report.Std

type t

val empty : t

val read : string -> t

val provide_kinds : t -> recipe -> recipe
