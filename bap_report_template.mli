open Bap_report_types

type artifact

module Artifact : sig
  type t = artifact

  val create : name:string -> size:string -> t
  val add_check : t -> check -> stat -> result -> t
  val name : t -> string
end

val render : artifact list -> string
