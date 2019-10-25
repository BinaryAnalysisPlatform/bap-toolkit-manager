open Core_kernel

(**

{2 Intro }

The whole purpose of the bap-toolkit is to provide an
easy access to BAP without deep understanding how to
build it, what configure options should be set,
what arguments should be fed and etc. Also, bap-toolkit is
an easy way to get a friendly report about incidents that
happened during the analysis.

As a result the whales, which bap-toolkit is standing on
(can't promise to keep exactly three of them) are:
- bap
- docker image with a set of analysis
- artifacts and recipes

And the flat surface that this whales are summoned to hold
is bap-toolkit with a fairly small set of options.


{3 Journaling }

BAP provides a tremendous number of options, and each of them
can alter results of an analysis. And recipes were added to
BAP universe to gain simplicity and reproducability.
Simplicity means that one need only a single command line
argument to get results, and reproducability
means that one can share the recipe with someone else to
get the same results.

Bap-toolkit made one more tiny step forward to reproducability.
BAP is alive creature that grows, becomes clever and more precise:
e.g. from some point of time a better algorithm for CFG reconstruction
comes on duty or a bug is fixed. Given all of that, it nearly
impossible to get a reasonable explanation, why results from the
same analysis can differ.

To make such kind of investigations easier, Bap-tookit records as much
of evidences of the running BAP instance as it can: time it take,
IR program, log, errors.

{3 Artifacts and incidents }

Incidents are the most precious outcome of any analysis.
And incidents can appear when a recipe(s) (analysis) is applied to
artifact(s).
Incidents are unique for every combination of (artifact + analysis):
they have a history - a set of addresses, i.e. a trace that leads to
the incident. This history is called Locations.
Status is bound with each incident and it's our relation to the
incident, if we believe in it or not.

*)

module Std : sig

  (** docker image  *)
  type image

    module Image : sig

      type t = image

      (** [of_string name] creates a new image from string.
          Returns Error if name isn't a proper docker image name.
          Image tag can be fed with ":" separator to designate tag. *)
      val of_string : string -> t Or_error.t

      (** [of_string_exn name] is same as [of_string] but raises
          Invalid_argument if name isn'y a proper docker image name. *)
      val of_string_exn : string -> t

      (** [exists image] returns true if the [image] exists. Returns false
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

  (** Tool is layer that incapsulate bap frontend and
      a list of recipes that it can run.

      One can think of a tool as of a container with bap and recipes. *)
  module Tool : sig
    type t

    (** [of_image im] creates tool from image.
        return Error if the provided image doesn't contain bap frontend*)
    val of_image : image -> t Or_error.t

    (** [host ()] creates tool from the host bap installation.
        return Error if bap not found *)
    val host : unit -> t Or_error.t

    (** [recipes tool ] returns the list of the recipes
        that are provided by [tool]. *)
    val recipes : t -> recipe list

    (** [find_recipe tool name] finds a recipe by its name. Return None if
        not recipe found *)
    val find_recipe : t -> string -> recipe option

    val bap_version : t -> string option

    val to_string : t -> string

  end

  module Recipe : sig

    type t = recipe

    (** [name recipe] returns the name of the recipe  *)
    val name : t -> string

    (** [description recipe] returns the description of the recipe *)
    val description : t -> string

    (** [add_parameter t ~name ~value] returns a recipe [t]
        with the parameter [name] bound to [value] *)
    val add_parameter : t -> name:string -> value:string -> t

    val to_string : t -> string

    (** [provide t kind] registers [t] as a provider of incidents of [kind] *)
    val provide : t -> incident_kind -> t

    (** [kinds r] returns a list of incidents kinds that
        can be happened during the recipe run *)
    val kinds : t -> incident_kind list

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

  module File : sig
    type t

    (** [create ?image path] create a path from
        a string. if [image] is set then path is
        considered relatively the image root,
        otherwise relatively the host root *)
    val create : ?image:image -> string -> t

    (** [size t] returns the size of the file at [path]. *)
    val size : t -> int option

  end

  type file = File.t
  type tool = Tool.t

  module Job : sig

    type t
    type ctxt

    (** [context ~verbose ~limit tool] creates a context for a
        job in order to make it easy to repeat different jobs
        under the same context.

        @param verbose saves bap BIR and asm output, true by default *)
    val context : ?verbose:bool -> ?limit:limit -> tool -> ctxt

    (** [run ctxt recipe path ] runs the [recipe] for the artifact at [path]  *)
    val run : ctxt -> recipe -> file -> t

    (** [time job] returns time  in seconds spent for the job [t] *)
    val time : t -> float

    (** [incidents t] returns a list of incidents *)
    val incidents : t -> incident list

    (** [errors t] returns a list of errors from the stderr *)
    val errors : t -> string list

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
      val locations : t -> locations
      val kind : t -> incident_kind
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

    (** [create ~file name] creates a new artifact. *)
    val create : ?file:file -> string -> t

    (** [update artifact incident status]
        updates the [artifact] with new data *)
    val update : t -> incident -> status -> t

    (** [no_incidents t kind] states that there weren't any
         incident of [kind] happened with artifact.  *)
    val no_incidents : t -> incident_kind -> t

    (** [incidents ~check artifact] returns the list of incidents
        that were happen with [artifact] along with the status.
        If [check] is set then returns only incident for the given [check] *)
    val incidents : ?kind : incident_kind -> t -> (incident * status) list

    (** [checks artifact] returns all the checks that were run
        against [artifact] *)
    val checks : t -> incident_kind list

    (** [name artifact] returns a name of the [artifact] *)
    val name   : t -> string

    (** [file artifact] returns a file that is corresponded to the [artifact]  *)
    val file   : t -> file option

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

    (** [merge a a'] returns an artifact that posses the information
        from both of artifacts [a] and [a'] if the following is true:
         - [a] and [a'] refers to the same artifact
         - there are no contradictions in the information, e.g.
           if can't be that an incident from [a] is defined as false
           negative and from [a'] as confirmed *)
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

  (** View is used to display results of an analysis. *)
  module View : sig

    type col =
      | Path of int (* deep  *)
      | Name
      | Addr
      | Locations
    [@@deriving sexp]

    type info =
      | Web of string
      | Col of col
      | Alias of string
    [@@deriving sexp]

    (** [register kind info] adds a new information about
        displaying incidents of [kind].  *)
    val register : incident_kind -> info -> unit

    (** [tab_of_incident i] return a string list
        of info associated with a given incident *)
    val tab_of_incident : incident -> string list
  end


  (** Few helpers for parsing incidents, confirmations and etc. *)
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
    val render : artifact list -> string
  end

end
