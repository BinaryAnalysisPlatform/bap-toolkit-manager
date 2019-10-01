type quantity = [ `S | `M | `H | `Mb | `Gb ]
type t

val empty : t

val add  : t -> int -> quantity -> t

val quantity_of_string : string -> quantity option
val string_of_quantity : quantity -> string
val string_of_t : t -> string
