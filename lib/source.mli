(** Sources of candidates *)

type 'a t_open = {
  delay: bool;
  (** Should we wait for the user to stop typing before trying to
      compute the candidates ? *)
  default_state: 'a;
  (** The state the source should start in. *)
  compute: 'a -> string -> ('a * Candidate.t list) ;
  (** computes the list of candidates and the new state out of the
      current state and the current user input. *)
}
(** A source is a way to produce candidates from the user input. *)

type t = S : 'a t_open  -> t
(** Existential closure of {!t_open} *)

type state = ST : 'a * 'a t_open -> state
(** A source and a state bundled together *)

val initialize : t -> state
(** Initialize a source to its default state *)

(** {2 Examples of sources} *)

val files : ?filter:(string -> bool) -> string -> t
(** A source that completes filenames in a given directory (or
    absolute filenames). 
    
    The [filter] function is used to exclude some files from the
    completion. It is given the absolute path.
*)

val from_list : (string * string * string) list -> t
(** A source built from a list of candidates of the form [(display,
    real, documentation)]. See {!Candidate.t} for more information about
    them. *)

val from_list_lazy : (string * string * string) list Lazy.t -> t
(** Same as {!from_list} but expects a non-evaluated list. *)

val from_list_lazy_ : string list Lazy.t -> t
(** Same as {!from_list_} but expects a non-evaluated list. *)

val from_list_rev : (string * string * string) list -> t
(** Same as {!from_list} but reverses the list in the process *)

val from_list_ : string list -> t
(** Same as {!from_list} but identifies real and display, and set
    documentation to the empty string. *)

val from_list_rev_ : string list -> t
(** Same as {!from_list_} but reverses its argument.*)

val binaries : t
(** A source that list all the binaries in your PATH. 

    The display is the binary name and the display its absolute path.
*)

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
    display and the second part as the real. *)

val update_candidates : (Candidate.t -> Candidate.t) -> t -> t
(** Update the candidates returned by this source *)
val update_matching : (Matching.t -> Matching.t) -> t -> t
(** Update the matching function of the candidates returned by this source *)
val update_completion : (string -> string) -> t -> t
(** Update the completion of the candidates returned by this source *)
val update_display : (string -> string) -> t -> t
(** Update the display of the candidates returned by this source *)
val update_real : (string -> string) -> t -> t
(** Update the real of the candidates returned by this source *)

