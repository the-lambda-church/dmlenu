(** Sources *)
open Completion

type t = Completion.ex_source

(** {3 Predefined sources and sources builder} *)

val files : ?filter:(string -> bool) -> string -> t
(** A source that completes filenames in a given directory (or absolute filenames) *)

val from_list : (string * string * string) list -> t
val from_list_rev : (string * string * string) list -> t
(** A source built from a list (display, real, documentation).
    See {!Completion.candidate} for more information about these. *)

val from_list_ : string list -> t
val from_list_rev_ : string list -> t
(** A source built from a list of candidates (display and real are the same, doc
    is empty). *)

val binaries : t
(** A source that completes a binary of the system *)

val switch : ((string -> bool) * t Lazy.t) list -> t
(** [switch guards] is a source that behaves like one of the sources in
    [guards]. To know what it source it behave like, it applies the
    predicate to the input and uses the first matching source. *)

val empty : t
(** The empty source. *)

val paths : coupled_with:t -> t
(** A source that complete like [coupled_with] except when what is before the
    cursor is (syntactically) a path, i.e. start with "/", "~/", or "./" *)

val stdin : ?sep: string -> unit -> t
(** A source that reads its elements off stdin. If separator is not
    supplied, then there is a candidate per line and the real and
    display fields of the candidate is the same.  If it is supplied,
    then lines are split wrt it and the first part is used as the
    display and the second part as the real *)

(** {2 An interactive source} *)

val add_subcommand : name:string -> t list Lazy.t -> unit
(** [add_subcommand ~name sources] adds [sources] under the name [name] in the
    list of subcommands used by [binaries_with_subcommands] *)

val set_default_subcommand_hook : (string -> t) -> unit
(** When [binaries_with_subcommands] doesn't find a subcommand of a given name
    [n] it will call the default subcommand hook with the string [n].

    The default behavior of this hook (i.e. if this function is never called) is
    to look for a file ["$HOME/.config/dmlenu/n"] and treat each line of this
    file as a completion candidate; if the file doesn't exists it behaves as
    [paths ~coupled_with:binaries]. *)

val binaries_with_subcommands : program
(** Behaves as [paths ~coupled_with:binaries] for the first completions, then
    looks in the subcommands list (see {!add_subcommand} and
    {!set_default_subcommand_hook}) for the following completions. *)
