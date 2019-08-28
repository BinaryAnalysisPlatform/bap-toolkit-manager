open Core_kernel
open Bap_report_types

module S () : sig
  type t
  val of_string : string -> t
  val to_string : t -> string
  include Identifiable.S with type t := t
end = struct

  let to_string x = x
  let of_string x = x

  include String
end

module Machine_id = S ()
module Location_id = S ()

type location_id = Location_id.t
type machine_id  = Machine_id.t

type event =
  | Incident_location of location_id * addr list
  | Incident of incident_kind * location_id list
  | Fork   of machine_id * machine_id
  | Switch of machine_id * machine_id
  | Call of string
  | Call_return of string
  | Symbol of string * addr
  | Pc_changed of addr
