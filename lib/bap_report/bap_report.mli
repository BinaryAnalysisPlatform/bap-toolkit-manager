open Core_kernel

module Std : sig

  module Docker : sig

    (** [image_exists image ~tag] returns true if
      the [image] with [tag] exists. Returns false
      otherwise (e.g. if tag is set, but image with such
      tag is not found. *)
    val image_exists : ?tag:string -> string -> bool


    (** [available_tags image] returns the list of all available tags
        for the given [image]. *)
    val available_tags : string -> string list

    (** [run ~image ~tag ~entry ~mount cmd] runs [cmd] with the docker
      [image].

      @param mount defines a mounting volume as a pair of pathes
             in the host and in the image.

      @param entry override an entrypoint for the [image] *)
    val run : image:string -> ?tag:string -> ?entry:string ->
              ?mount:string * string -> string -> string option

    (** [pull image ~tag] pulls the [image] with [tag] *)
    val pull : ?tag:string -> string -> unit

    (** [get_image ~tag image] ensures that the [image]
        with the given [tag] is available locally. If there is
        no such image, then it will try to pull it *)
    val get_image : ?tag:string -> string -> unit Or_error.t

  end

  type tool
  type recipe
  type artifact [@@deriving bin_io, compare, sexp]
  type addr [@@deriving bin_io,compare,sexp]

  type status =
    | Confirmed
    | False_pos
    | False_neg
    | Undecided
   [@@deriving bin_io, compare, sexp]

  type stat = {
      false_pos : int;
      false_neg : int;
      confirmed : int;
      undecided : int;
   }[@@deriving bin_io, compare, sexp]

  type locations     [@@deriving bin_io, compare, sexp]
  type incident      [@@deriving bin_io, compare, sexp]
  type incident_kind [@@deriving bin_io, compare, sexp]
  type incident_id   [@@deriving bin_io, compare, sexp]
  type confirmation  [@@deriving bin_io, compare, sexp]


  (** Tool is a docker image that responsible for running recipes *)
  module Tool : sig

    type t = tool

    (** [of_string name] creates a new tool from string.
        Returns Error if name can't be a proper docker image name.
        Image tag can be fed with ":" separator. *)
    val of_string : string -> t Or_error.t

    val name : t -> string
    val tag  : t -> string option
  end


  module Recipe : sig

    type t = recipe

    (** [find tool name] finds a recipe by its name. Return None if
        not recipe found *)
    val find : tool -> string -> t option

    (** [list tool ] returns the list of the recipes
        that are provided by [tool]. *)
    val list : tool -> t list

    (** [name recipe] returns the name of the recipe  *)
    val name : t -> string

    (** [description recipe]returns the description of the recipe *)
    val description : t -> string

    (** [run ~tool ~image ~tag path recipe] runs the recipe.
      if [image] and/or [tag] is set then [path] is considered
      relatively to the [image], else to the host filesystem  *)
    val run : tool:tool -> ?image:string -> ?tag:string -> string -> t -> t

    (** [time_taken recipe] returns a time that was spent for
      the last {run} command *)
    val time_taken : t -> float

  end

  module Size : sig

   (** [get ?image ?tag path] returns the size of the file at [path].
      if [image] and/or [tag] is set then [path] is considered
      relatively to the [image], else to the host filesystem*)
    val get : ?image:string -> ?tag:string -> string -> int option

  end

  module Addr : sig
    type t = addr [@@deriving bin_io,compare,sexp]

    val of_string : string -> t
    val to_string : t -> string

    include Identifiable.S   with type t := t
  end

  module Locations : sig

    type t = locations [@@deriving bin_io, compare, sexp]

    val create : ?prev : addr list -> addr -> t
    val addrs : t -> addr list

    include Identifiable.S   with type t := t
  end

  (* TODO: document it and don't forget to say that
     locations in [create] are not the same as
     incident location. That they are first addresses
     of all locations for incident *)
  module Incident : sig

    module Kind : sig
      type t = incident_kind  [@@deriving bin_io, compare, sexp]

      val of_string : string -> t
      val to_string : t -> string

      include Identifiable.S   with type t := t
    end

    module Id : sig
      type t = incident_id  [@@deriving bin_io, compare, sexp]

      val create : incident_kind -> locations -> t

      include Identifiable.S   with type t := t
    end

    type t = incident [@@deriving bin_io, compare, sexp]

    val create : ?path:string list -> locations -> incident_kind -> t

    val of_id : incident_id -> t

    val addr : t -> addr
    val locations : t -> locations

    (** [path inc] return a list of predcessing calls before incident
        happened, the most recent is a head of a list  *)
    val path : t -> string list
    val kind : t -> incident_kind
    val id   : t -> incident_id

    include Identifiable.S   with type t := t
  end

  module Artifact : sig

    type t = artifact [@@deriving bin_io, compare, sexp]

    (** [create ~size name] creates a new artifact. *)
    val create : ?size:int -> string -> t

    (** [update artifact incident status]
        updates the [artifact] with new data *)
    val update : t -> incident -> status -> t

    (** [incidents ~check artifact] returns the list of incidents
        that were happen with [artifact] along with the status.
        If [check] is set then returns only incident for the given [check] *)
    val incidents : ?kind : incident_kind -> t -> (incident * status) list

    (** [checks artifact] returns all the checks that were run
        against [artifact] *)
    val checks : t -> incident_kind list

    (** [name artifact] returns a name of the [artifact] *)
    val name   : t -> string

    (** [size artifact] return a size (in bytes) of the [artifact]*)
    val size   : t -> int option

    (** [size_hum artifact] returns a human readable string
        with size of the [artifact] *)
    val size_hum :  t -> string option

    (** [with_size artifact size] updates a size of the [artifact]*)
    val with_size : t -> int -> t

    (** [time artifact check] returns a time taken to run
        [check] against [artifact] in seconds *)
    val time : t -> incident_kind -> float option

    (** [time_hum artifact check] returns a time taken to run
        [check] against [artifact] in human readable
        format *)
    val time_hum : t -> incident_kind -> string option

    (** [with_time artifact check time] updates time *)
    val with_time : t -> incident_kind -> float -> t

    (** [summary artifact check] returns a summary for the given [check] *)
    val summary : t -> incident_kind -> stat

    (** [find artifact id] returns an incident with [id]
        along with its status, if any found *)
    val find : t -> incident_id -> (incident * status) option

  end


  (** Confirmation is a expected incident and it could be
      either of two kinds:

    - must - such one which MUST come up during the analysis
      and therefore an absence of such incindent is False negative,
      and a presence is a Confirmed incident

    - may - such one which MAY come up during the analysis.
      and therefore an absence of such incindent is not a mistake,
      and a presence is False positive.  *)
  module Confirmation : sig

    type t = confirmation [@@deriving sexp,compare,bin_io]

    type kind [@@deriving sexp,compare,bin_io]

    val must : kind
    val may  : kind

    (** [create kind incident_kind locations]
        creates a confirmation of [kind] for incidents of
        [incident_kind] and [locations]. Note, that locations
        are unambigously define an incident of [incident_kind],
        and will be use to confirm incidents *)
    val create : kind -> incident_kind -> locations -> t

    val id : t -> incident_id


    val locations : t -> locations
    val incident_kind : t -> incident_kind
    val confirmation : t -> kind

    val is : kind -> t -> bool

    (** [validate conf status] - for a given
        confirmation and incident status returns
        a confirmed status of the incident. None
        for [status] means that incident didn't
        happen during the analysis. *)
    val validate : t -> status option -> status

  end

  module View : sig

    type col =
      | Path of int (* deep  *)
      | Name
      | Addr
      | Locations

    type info =
      | Web of string
      | Tab of col list
      | Alias of string

    type t

    val create : unit -> t
    val update : t -> incident_kind -> info -> unit
    val name : t -> incident_kind -> string
    val web  : t -> incident_kind -> string option
    val data : t -> incident -> string list
    val of_file : string -> t

  end

  type view = View.t

  module Read : sig

    (** [incidents channel] read incidents from [channel]  *)
    val incidents : In_channel.t -> incident list

    (** [confirmations channel] returns a list of confirmed
        incidents associated with the name of artifact *)
    val confirmations :
      In_channel.t -> (string * confirmation list) list

  end

  module Template : sig

    (** [render view artifacts] retutns the html report  *)
    val render : view -> artifact list -> string
  end

end
