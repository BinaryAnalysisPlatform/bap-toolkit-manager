
val image_exists : ?tag:string -> string -> bool

val available : unit -> (string * string option) list

val run : image:string -> ?tag:string -> ?entry:string ->
          ?mount:string * string -> string -> string option

val pull : ?tag:string -> string -> unit
