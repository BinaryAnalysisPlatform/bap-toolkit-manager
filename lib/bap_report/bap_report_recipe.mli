open Core_kernel

type t

val find : string -> t option

val list : unit -> t list

val name : t -> string

val description : t -> string

val run : ?image:string -> ?tag:string -> string -> t -> t

val time_taken : t -> float
