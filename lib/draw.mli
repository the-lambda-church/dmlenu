(** Drawing the window *)

type window = {
  state: X.state; (** X state *)
  prompt: string; (** Prompt to display *)
  input: string; (** Current user input *)
  hcandidates: (Candidate.t * Matching.result) Pagination.t; (** Horizontal candidates *)
  vcandidates: (Candidate.t * Matching.result * string) Pagination.t; 
(** Vertical candidates (the extra string is the doc) *)
}

val split: window: window -> vertical: bool -> Candidate.t list -> Candidate.t list * Candidate.t list
(** Splits a list of candidates into the one that can be displayed and the other ones *)

val draw: window: window -> unit
(** Draw the window *)



