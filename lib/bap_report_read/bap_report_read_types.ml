open Core_kernel
open Bap_report_types

type location_id = string
type machine_id = string
type addr = string

module Machine_id = String
module Location_id = String
module Addr = String


type event =
  | Incident_location of location_id * addr list
  | Incident of string * location_id list
  | Fork   of machine_id * machine_id
  | Switch of machine_id * machine_id
  | Call of string
  | Call_return of string
  | Symbol of string * addr
  | Pc_changed of addr
