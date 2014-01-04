(** Sources *)
open Completion

type t = Completion.ex_source

(** {3 Predefined sources and sources builder} *)

val files : ?filter:(string -> bool) -> string -> t
(** A source that completes filenames in a given directory (or absolute filenames). *)

val from_list : (string * string * string) list -> t
val from_list_rev : (string * string * string) list -> t
(** A source built from a list (display, real, documentation).
    See {!Completion.candidate} for more information about these. *)

val from_list_ : string list -> t
val from_list_rev_ : string list -> t
(** A source built from a list of candidates (display and real are the same, doc
    is empty). *)

val csum : (string * state_machine Lazy.t) list -> state_machine
(** Simple case of dependant sum *)

val binaries : t
(** A source that list all the binaries in your PATH. *)

val switch : ((string -> bool) * t Lazy.t) list -> t
(** [switch guards] is a source that behaves like one of the sources in
    [guards]. To know what it source it behave like, it applies the
    predicate to the input and uses the first matching source. *)

val empty : t
(** The empty source. *)

val paths : coupled_with:t -> t
(** A source that complete like [coupled_with] except when what is before the
    cursor is (syntactically) a path, i.e. start with "/", "~/", or "./" *)

val stdin : ?sep:string -> unit -> t
(** A source that reads its elements off stdin. If separator is not
    supplied, then there is a candidate per line and the real and
    display fields of the candidate is the same.  If it is supplied,
    then lines are split wrt it and the first part is used as the
    display and the second part as the real *)
