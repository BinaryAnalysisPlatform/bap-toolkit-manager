open Core_kernel

type t [@@deriving bin_io, compare, sexp]

val of_string : string -> t Or_error.t
val of_string_exn : string -> t
val to_string : t -> string
val exists : t -> bool
val tags : t -> string list
val pull : t -> unit
val get : t -> unit Or_error.t
val with_tag: t -> string -> t
val name : t -> string
val tag  : t -> string option
val run : ?entry:string ->
  ?mount:string * string -> t -> string -> string option
