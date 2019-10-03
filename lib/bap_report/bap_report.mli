open Core_kernel

module Std : sig

  (** docker image  *)
  type image

  module Docker : sig
    module Image : sig

      type t = image

      (** [of_string name] creates a new image from string.
          Returns Error if name isn't a proper docker image name.
          Image tag can be fed with ":" separator to designate tag. *)
      val of_string : string -> t Or_error.t

      (** [of_string_exn name] is same as [of_string] but raises
          Invalid_argument if name isn'y a proper docker image name. *)
      val of_string_exn : string -> t

      (** [exists image] returns true if
          the [image] exists. Returns false
          otherwise (e.g. image exists, but image's tag is not found *)
      val exists : t -> bool

      (** [tags image] returns the list of all available tags
          for the given [image]. *)
      val tags : t -> string list

      (** [pull image] pulls the [image] with [tag] *)
      val pull : t -> unit

      (** [get_image image] ensures that the [image]
          is available locally. If there is
          no such image, then it will try to pull it *)
      val get : t -> unit Or_error.t

      (** [with_tag t tag] return the image [r] with a new [tag] *)
      val with_tag: t -> string -> t

      (** [name im] return name of the image [im] *)
      val name : t -> string

    end

    (** [run ~entry ~mount image cmd] runs [cmd] with the docker [image].

        @param mount defines a mounting volume as a pair of pathes
             in the host and in the image.

        @param entry override an entrypoint for the [image] *)
    val run : ?entry:string ->
      ?mount:string * string -> image -> string -> string option

  end

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

  module Recipe : sig

    type t = recipe

    (** [find tool name] finds a recipe by its name. Return None if
        not recipe found *)
    val find : image -> string -> t option

    (** [list tool ] returns the list of the recipes
        that are provided by [tool]. *)
    val list : image -> t list

    (** [name recipe] returns the name of the recipe  *)
    val name : t -> string

    (** [description recipe] returns the description of the recipe *)
    val description : t -> string

    (** [add_parameter t ~name ~value] returns a recipe [t]
        with the parameter [name] bound to [value] *)
    val add_parameter : t -> name:string -> value:string -> t

    val to_string : t -> string
  end

  module Limit : sig
    type quantity = [
      | `S  (** seconds  *)
      | `M  (** minutes *)
      | `H  (** hours *)
      | `Mb (** megabytes *)
      | `Gb (** gigabytes *)
    ]

    type t

    val empty : t

    (** [add num quantity] adds a new limit *)
    val add  : t -> int -> quantity -> t

    val quantity_of_string : string -> quantity option
    val string_of_quantity : quantity -> string

  end

  type limit = Limit.t

  module Job : sig

    type t

    (** [run recipe ~tool ~verbose=true ~image ~limit path ] runs the recipe.
        [tool] is an image responsible for running the recipe.
        if [image] is set then [path] is considered
        relatively to the [image], else to the host filesystem.
        returns a time that was spent to process the recipe.

        @param verbose saves bap BIR and asm output, true by default *)
    val run : recipe -> tool:image -> ?verbose:bool -> ?image:image -> ?limit:limit -> string -> t

    (** [time job] returns time  in seconds spent for the job [t] *)
    val time : t -> float

    (** [incidents t] returns a list of incidents *)
    val incidents : t -> incident list

    (** [errors t] returns a list of errors from the stderr *)
    val errors : t -> string list

  end


  module Size : sig

    (** [get ?image path] returns the size of the file at [path].
        if [image] is set then [path] is considered
        relatively to the [image], else to the host filesystem*)
    val get : ?image:image -> string -> int option

  end

  module Addr : sig
    type t = addr [@@deriving bin_io,compare,sexp]

    val of_string : string -> t
    val to_string : t -> string

    include Identifiable.S   with type t := t
  end

  (** Locations is a non-empty sequence of addresses.
      E.g. for use-after-free incident locations
      will be contain three addresses:
      - address of memory allocation
      - address of memory free
      - address of usage after free.
        The last one is considered as incident address *)
  module Locations : sig

    type t = locations [@@deriving bin_io, compare, sexp]

    val create : ?prev : addr list -> addr -> t
    val addrs : t -> addr list

    include Identifiable.S   with type t := t
  end

  (** Incident is an event happened during the analysis.
      It is defined unambigously by it's locations and
      it's kind. The kind of the incident is defined
      by each analysis independently, e.g.
      - null-pointer-dereference
      - use-after-free
      - recursive-function *)
  module Incident : sig

    module Kind : sig
      type t = incident_kind  [@@deriving bin_io, compare, sexp]
      include Identifiable.S   with type t := t
    end

    module Id : sig
      type t = incident_id  [@@deriving bin_io, compare, sexp]
      val create : incident_kind -> locations -> t
      include Identifiable.S   with type t := t
    end

    type t = incident [@@deriving bin_io, compare, sexp]

    (** [create path locs kind] creates a new incident.
        [path] is a sequence of function calls that leds to
        incident, need just for display purposes. *)
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

  (** artifact is an object of analysis.  *)
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

    val merge : t -> t -> t option

  end


  (** Confirmation is an expected incident and it could be
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
    val is : t -> kind -> bool

    (** [validate conf status] - for a given
        confirmation and incident status returns
        a confirmed status of the incident. None
        for [status] means that incident didn't
        happen during the analysis. *)
    val validate : t -> status option -> status

  end

  (** View describes a way to display the results of an analysis.  *)
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
    val confirmations : In_channel.t -> (string * confirmation list) list

    module Helper : sig

      (** [words ?comments channel] returns a list of lines, where
          each line is splitted on words.
          if [comments] is set, then lines with such prefix will be
          ignored. Empty lines are ignored too.*)
      val words : ?comments:string -> In_channel.t -> string list list

      (** [lines ~comments channel]  returns a list of lines.
          if [comments] is set, then lines with such prefix will be
          ignored. Empty lines are ignored too. *)
      val lines :  ?comments:string -> In_channel.t -> string list

    end
  end

  module Template : sig

    (** [render view artifacts] retutns the html report  *)
    val render : view -> artifact list -> string
  end

end
