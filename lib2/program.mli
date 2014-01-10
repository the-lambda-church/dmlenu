(** Programs and actual completion *)

type 'a state_open = {
  source: 'a Source.t_open;
  state: 'a;
  candidates: candidate list;
  before_matches: (candidate * Matching.result) list;
  after_matches: (candidate * Matching.result) list;
}
(** The state for the completion on a source. It contains the current
    candidates and its views *)

type state = ST : 'a state_open -> state
  
