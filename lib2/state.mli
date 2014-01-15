(** State of the completion engine *)

type t = {
  separator: string; 
  before_cursor: string;
  after_cursor : string;
  program: Program.t;
  sources : Source.state list; 
  candidates: (Candidate.t * Matching.result) Pagination.t;
  compl_sources : Source.state list;
  compl_candidates: (Candidate.t * Matching.result) Pagination.t;
  entries: (Program.t * Candidate.t) list;
  splith: (Candidate.t * Matching.result) list -> (Candidate.t * Matching.result) list * (Candidate.t * Matching.result) list;
}

(** The type of state *)

val initial : separator: string -> program: Program.t -> 
  splith: ((Candidate.t * Matching.result) list -> (Candidate.t * Matching.result) list * (Candidate.t * Matching.result) list) -> t
(** Initial state *)

val on_modify : t -> t
(** Function to be called whenever the input is modified *)

val add_char : t -> string -> t * Candidate.t option
(** Add a char to the current input. Returns the selected candidate if
    the char makes the completion of the current token finished. *)

val complete : t -> t * Candidate.t option
(** Tries to complete the current selected candidate. If the current
    token is done, returns the selected candidate *)
