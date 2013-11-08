(** Completion engine *)

(** This is the interface of the completion engine.

    The completion engine works with {!source}s that returns {!candidate}s. *)
type match_result = ((bool * int * int) list) option
type candidate = {
  display: string;
  (** The string that should be displayed for the candidate *)
  real: string;
  (** The string that will be printed out if this candidate is picked *)
  matching_function: (string -> string -> match_result);
  (** How to tell if the candidate is matching the current input.
      Should take the string before and after the cursor and return a
      list of matching and non matching position in the string *)
  completion_function: string -> string -> string * string;
  (** Takes the string before and after the point and return the new
      string before and after the cursor, after completing this
      candidate *)
}
(** A candidate, as returned by sources *)



(** {3 Sources} *)
type 'a source = {
  delay: bool;
  (** Should we wait for a delay before computing candidates ? *)
  default: 'a;
  (** Default value for the state *)
  compute: 'a -> string -> string -> 'a * candidate list;
  (** computes takes the current state, the string before/after the
      cursor and returns the new state along with a list of candidate *)
}

type ex_source = S : 'a source -> ex_source
(** Existential closure of {!source} *)

type source_state = ST : 'a * 'a source -> source_state

(** {2 State} *)
type state = {
  before_cursor: string;
  after_cursor: string;
  sources: (candidate list * source_state) list;
  matches: (candidate * (bool * int * int) list) list;
}
(** The state of the completion engine *)

val make_state: ex_source list -> state
(** Creates an initial state out of a list of sources *)

val add_char : char -> state -> state
(** Computes the new state corresponding to the user pressing a character *)

(** {3 Edition commands} *)
val left : state -> state
(** Computes the new state corresponding to the user going left *)

val right : state -> state
(** Computes the new state corresponding to the user going right *)

val remove : state -> state
(** Computes the new state corresponding to the user removing the
    character to the right of the cursor *)
val complete : state -> state
(** Computes the new state corresponding to the user completing the entry *)
