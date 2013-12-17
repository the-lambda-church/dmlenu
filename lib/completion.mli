(** Completion engine *)

(** This is the interface of the completion engine.

    The completion engine works with {!source}s that returns {!candidate}s.
    See {!state_machine} *)


type candidate = <
  display: string;
  (** The string that should be displayed for the candidate *)
  real: string;
  (** The string that will be printed out if this candidate is picked *)
  doc : string;
  (** A "documentation" string that will be printed on the right of the
      candidate if the window takes more than 1 line. *)
  completion: string;
  (** The string to use when this candidate is completed *)
  matching_function: (string -> Matching.result option);
  (** How to tell if the candidate is matching the current input. *)
>
(** A candidate, as returned by sources *)


val mk_candidate :
  display:string ->
  real:string ->
  completion:string ->
  doc:string ->
  matching_function:(string -> Matching.result option) ->
  candidate
(** A constructor *)

(** {3 Sources} *)

type 'a source = {
  delay: bool;
  (** Should we wait for a delay before computing candidates ? *)
  default: 'a;
  (** Default value for the state *)
  compute: 'a -> string -> 'a * candidate list;
  (** computes takes the current state, the string before/after the
      cursor and returns the new state along with a list of candidate *)
}

type ex_source = S : 'a source -> ex_source
(** Existential closure of {!source} *)

type source_state = ST : 'a * 'a source -> source_state

(** {3 Engine} *)

type state_machine = {
  ex_sources : ex_source Lazy.t list ;
  transition : < display:string ; real:string > -> state_machine
}
(** A completion state machine: a list of current source and a way to get
    the next sources depending on the current input. *)

val dummy_machine : state_machine
(** The state machine that does not offer any completion *)

val empty : state_machine
(** Same as {!dummy_machine} *)

val concat : state_machine -> state_machine -> state_machine
(** Concatenates two machines. The first one is considered done when
    it returns []. *)

val sum : ex_source -> (<display: string; real: string> -> state_machine) -> state_machine
(** Dependant sum. *)

val singleton : ex_source -> state_machine
(** Offers the argument for completion once and then do not provide any completion *)

val iterate : ex_source Lazy.t list -> state_machine
(** [iterate sources] offers completions from sources, indefinitely. *)

(** {2 State} *)

type state = {
  before_cursor: string;
  after_cursor: string;
  sources: (candidate list * source_state) list;
  before_matches: (candidate * Matching.result) list;
  after_matches: (candidate * Matching.result) list;
  entries: (state_machine * string * string) list;
  separator : string;
  program: state_machine;
}
(** The state of the completion engine *)

val make_state: ?sep:string -> state_machine -> state
(** [make_state ?sep machine] initialize the machine; [sep] will be used to mark
    the end of a selection and the transition to the next sources (i.e. next
    state of the automata).

    The default value of [sep] is a space. *)

val add_string : string -> state -> state
(** Computes the new state corresponding to the user pressing a character *)

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
