open Batteries

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

let on_modify state = 
  let input = state.before_cursor ^ state.after_cursor in
  let up_sources sources = 
    let r = ref [] in
    let sources = sources |> List.map 
        (fun (Source.ST (state, s)) ->
          let state', candidates = s.Source.compute state input in
          let test c = Option.map (fun x -> c, x) (c#matching_function input) in
          r := !r @ List.filter_map test candidates; (* !r is empty most of the times *)
          Source.ST (state', s))
    in
    sources, Pagination.from_list state.splith (List.rev !r)
  in
  let sources, candidates = up_sources state.sources
  and compl_sources, compl_candidates = up_sources state.compl_sources in
  { state with sources; candidates; compl_sources; compl_candidates }

let initial ~separator ~program ~splith = 
  let initialize (Source.S s) = Source.(ST (s.default_state, s)) in
  on_modify {
  splith; separator; program;
  entries = []; candidates = Pagination.from_list splith []; 
  compl_candidates = Pagination.from_list splith [];
  before_cursor = ""; after_cursor = "";
  sources = List.map initialize program.Program.sources;
  compl_sources = List.map initialize program.Program.completion;
}

let add_char state s = 
  on_modify { state with before_cursor = state.before_cursor ^ s }, None

let complete state = state, None
