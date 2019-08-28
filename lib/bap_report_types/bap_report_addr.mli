open Core_kernel


type t [@@deriving bin_io,compare,hash,sexp]

val of_string : string -> t
val to_string : t -> string


include Identifiable.S   with type t := t
