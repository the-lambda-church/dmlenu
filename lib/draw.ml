
type window = {
  state: X.state; (** X state *)
  prompt: string; (** Prompt to display *)
  input: string; (** Current user input *)
  hcandidates: (Candidate.t * Matching.result) Pagination.t; (** Horizontal candidates *)
  vcandidates: (Candidate.t * Matching.result * string) Pagination.t; 
(** Vertical candidates (the extra string is the doc) *)
}

let split
