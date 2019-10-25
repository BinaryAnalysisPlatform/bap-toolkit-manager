
include Bap_report_common

module Incident = Bap_report_incident
module Locations = Incident.Locations
module Artifact = Bap_report_artifact
module Confirmation = Bap_report_confirmation
module Image = Bap_report_image
module File = Bap_report_file

type artifact = Artifact.t [@@deriving bin_io,compare,sexp]
type incident = Incident.t [@@deriving bin_io,compare,sexp]
type incident_kind = Incident.kind [@@deriving bin_io,compare,sexp]
type confirmation = Confirmation.t [@@deriving bin_io,compare,sexp]
type confirmation_kind = Confirmation.kind [@@deriving bin_io,compare,sexp]
type locations = Locations.t [@@deriving bin_io,compare,sexp]
type incident_id = Incident.id [@@deriving bin_io,compare,sexp]
type image = Image.t
type file = File.t
