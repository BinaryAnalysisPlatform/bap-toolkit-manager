

type t

val of_file     : string -> t option
val elapsed     : t -> float option
val user_time   : t -> float option
val system_time : t -> float option
val command     : t -> string option
val cpu_percentage : t -> int option
