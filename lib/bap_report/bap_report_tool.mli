open Core_kernel

type t

val of_string : string -> t Or_error.t

val tag : t -> string option
val name : t -> string
