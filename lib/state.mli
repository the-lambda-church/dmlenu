(** State of the completion engine *)

(** This module defines the completion engine used by dmlenu *)

type t = {
  separator: string; (** The separator between tokens *)
  before_cursor: string; (** The input before the cursor *)
  after_cursor : string; (** The input after the cursor *)
  program: Program.t; (** The current program we are running *)
  sources : Source.state list; (** The current sources of tokens *)
  candidates: (Candidate.t * Matching.result) Pagination.t; (** The current candidates for tokens *)
  compl_sources : Source.state list; (** The current sources for completion *)
  compl_candidates: (Candidate.t * Matching.result) Pagination.t; (** The current candidates for completion *)
  entries: (Program.t * Candidate.t) list; (** The tokens we have read so far and the past program so we can go back there if we need to. *)
  splitm: (Candidate.t * Matching.result) list -> (Candidate.t * Matching.result) list * (Candidate.t * Matching.result) list;
  (** How to know how many token candidates we can display *)
  splitc: (Candidate.t * Matching.result) list -> (Candidate.t * Matching.result) list * (Candidate.t * Matching.result) list;
  (** How to know how many completion candidates we can display *)
}
(** The type of state the engine *)

val initial : separator: string -> program: Program.t -> 
  splitm: ((Candidate.t * Matching.result) list -> (Candidate.t * Matching.result) list * (Candidate.t * Matching.result) list) ->
  splitc: ((Candidate.t * Matching.result) list -> (Candidate.t * Matching.result) list * (Candidate.t * Matching.result) list) -> t
(** Creates an initial state out of a separator a program, and split functions *)

val on_modify : t -> t
(** Function to be called whenever the input is modified to recompute
    candidates. It will reset the selected candidates too. *)

val add_char : string -> t -> t * bool
(** Add a char to the current input. If the char is a separator and we
    are done completing, it also completes the token, in that case the boolean returned is true*)

val complete : t -> t * bool
(** Tries to complete the current selected candidate. Returns true whenever we moved to a new token. *)

val left : t -> t
(** Moves the current selection to the left *)

val right : t -> t
(** Moves the current selection to the right *)
(** Moves right *)

val up : t -> t
(** Moves the current selection to the up *)

val down : t -> t
(** Moves the current selection to the down *)

val remove : t -> t * bool
(** Simulate a backspace. The boolean tells you whether the current token has changed *)

val get_list : t -> string list
(** Returns the value of all tokens currently accumulated *)
