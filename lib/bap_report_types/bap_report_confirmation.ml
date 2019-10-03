open Core_kernel
open Bap_report_common

type kind =
  | Must
  | May
[@@deriving sexp,compare,bin_io]

type locs = Bap_report_incident.locations [@@deriving sexp,compare,bin_io]

type t = {
  locs : locs;
  kind : Bap_report_incident.kind;
  conf : kind;
} [@@deriving sexp,compare,bin_io]


let must = Must
let may  = May

let create conf kind locs = {locs; kind; conf}
let locations t = t.locs
let incident_kind t = t.kind
let confirmation t = t.conf
let id t = Bap_report_incident.Id.create t.kind t.locs

let validate {conf} status =
  match conf,status with
  | May, None -> Undecided
  | May, Some _ -> False_pos
  | Must, None -> False_neg
  | Must, Some _ -> Confirmed

let is {conf} conf' = conf' = conf
