

type t

type message = string list

val of_file : string -> t option

val errors : t -> message list
