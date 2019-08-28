open Core_kernel

type t
type tool = Bap_report_tool.t

val find : tool -> string -> t option

val list : tool -> t list

val name : t -> string

val description : t -> string

val run : tool:tool -> ?image:string -> ?tag:string -> string -> t -> t

val time_taken : t -> float
