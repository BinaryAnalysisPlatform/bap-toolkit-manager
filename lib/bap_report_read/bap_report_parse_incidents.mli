
open Bap_report_types
open Bap_report_read_types

val read : in_channel -> event option

val read_confirmations : in_channel -> (incident * status) list
