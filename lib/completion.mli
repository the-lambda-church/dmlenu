(** Completion engine *)

(** This is the interface of the completion engine.

    The completion engine works with {!source}s that returns {!candidate}s. *)
type candidate = {
  display: string;
  (** The string that should be displayed for the candidate *)
  real: string;
  (** The string that will be printed out if this candidate is picked *)
  completion: string;
  (** The string to use when this candidate is completed *)
  documentation: string list Lazy.t;
  (** The documentation of the candidate.  It might be displayed below
      the input line depending on the configuration. *)
  matching_function: (query: string -> Matching.result option);
  (** How to tell if the candidate is matching the current input. *)
}
(** A candidate, as returned by sources *)



(** {3 Sources} *)
type 'a source = {
  delay: bool;
  (** Should we wait for a delay before computing candidates ? *)
  default: 'a;
  (** Default value for the state *)
  compute: 'a -> (string * string) list -> string -> 'a * candidate list;
(** computes takes the already matched entries (real, display), the current entry,
    the string before/after the cursor and returns the new state
    along with a list of candidate *)
}

type ex_source = S : 'a source -> ex_source
(** Existential closure of {!source} *)

type source_state = ST : 'a * 'a source -> source_state

type program = Program of ex_source list * (string -> string -> program)
(** A completion program: a list of current source and a way to get
    the next sources depending on the current input (real, display) *)
(** {2 State} *)
type state = {
  before_cursor: string;
  after_cursor: string;
  sources: (candidate list * source_state) list;
  before_matches: (candidate * Matching.result) list;
  after_matches: (candidate * Matching.result) list;
  entries: (program * string * string) list;
  program: program;
}
(** The state of the completion engine *)

val make_state: program -> state
(** Creates an initial state out of a program *)

val add_string : string -> state -> state
(** Computes the new state corresponding to the user pressing a character *)

val empty_program : program
(** The program that does not offer any completion *)

(** {3 Edition commands} *)
val cursor_left : state -> state
(** Computes the new state corresponding to the user going left *)

val cursor_right : state -> state
(** Computes the new state corresponding to the user going right *)

val left : state -> state
(** Computes the new state corresponding to the user selecting the candidate on the left *)

val right : state -> state
(** Computes the new state corresponding to the user selecting the candidate on the right *)

val pageup : 
  ((candidate * Matching.result) list -> 
   (candidate * Matching.result) list * (candidate * Matching.result) list) ->
  state -> state
(** Computes the new state corresponding to the user selecting the
    first non visible candidate on the left. It expects a functions
    that given a list of candidate returns the list of the candidate
    visible on the screen and the rest. *)

val pagedown : 
  ((candidate * Matching.result) list -> 
   (candidate * Matching.result) list * (candidate * Matching.result) list) -> 
  state -> state
(** Computes the new state corresponding to the user selecting the first non visible candidate on the right *)

val remove : state -> state
(** Computes the new state corresponding to the user removing the
    character to the right of the cursor *)
val complete : state -> state
(** Computes the new state corresponding to the user completing the entry *)
