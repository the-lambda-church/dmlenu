open Batteries
open Candidate

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
  split: (Candidate.t * Matching.result) list -> (Candidate.t * Matching.result) list * (Candidate.t * Matching.result) list;
  lines: int;
}

let is_vertical { lines } = lines > 0
let take lines a = 
  if List.length a < lines then a, []
  else List.split_at lines a

let split_function state = 
  if is_vertical state then
    take state.lines
  else
    state.split

let on_modify state = 
  let input = state.before_cursor ^ state.after_cursor in
  let up_sources split sources = 
    let r = ref [] in
    let sources = sources |> List.map 
        (fun (Source.ST (state, s)) ->
          let state', candidates = s.Source.compute state input in
          let test c = Option.map (fun x -> c, x) (c.matching_function input) in
          r := !r @ List.filter_map test candidates; (* !r is empty most of the times *)
          Source.ST (state', s))
    in
    sources, Pagination.from_list split !r
  in
  let sources, candidates = up_sources (split_function state) state.sources
  and compl_sources, compl_candidates = up_sources state.split state.compl_sources in
  { state with sources; candidates; compl_sources; compl_candidates }


let initial ~separator ~program ~lines ~split = 
  on_modify {
    split; lines; separator; program;
    entries = []; candidates = Pagination.from_list (if lines > 0 then take lines else split) [];
    compl_candidates = Pagination.from_list split [];
    before_cursor = ""; after_cursor = "";
    sources = List.map Source.initialize program.Program.sources;
    compl_sources = List.map Source.initialize program.Program.completion;
}


let compl_candidates state = 
  if is_vertical state then state.compl_candidates
  else state.candidates

let next_entry candidate state =
  let f = state.program.Program.transition in
  let ({ Program.sources ; _ } as program) =
    f candidate
  in
  on_modify { state with
    before_cursor = "";
    after_cursor = "";
    sources = List.map Source.initialize sources;
    compl_sources = List.map Source.initialize program.Program.completion;
    candidates = Pagination.from_list (split_function state) [];
    compl_candidates = Pagination.from_list state.split [];
    separator = state.separator;
    program;
    entries = state.entries @ [state.program, candidate]
  }
let complete state = 
  try
    let candidate = fst (Pagination.selected (compl_candidates state))
    in
    let state' = on_modify {
      state with before_cursor = candidate.completion; after_cursor = ""
    } in
    if Pagination.all (compl_candidates state') |>
        List.exists (fun (x, _) -> x.real = candidate.real) 
    then
      next_entry candidate state, true
    else
      state', false
  with Failure _ ->
    state, false

let add_char s state = 
  let state' = 
    on_modify { state with before_cursor = state.before_cursor ^ s } 
  in
  try
    let first = fst @@ Pagination.selected (compl_candidates state) in
    if
      s = state.separator && state.after_cursor = "" &&
      state.before_cursor ^ state.after_cursor = first.display
    then
      complete state
    else
      state', false
  with _ ->
    state', false




let left state = 
  if is_vertical state then
    { state with compl_candidates = Pagination.left state.compl_candidates }
  else
    { state with candidates = Pagination.left state.candidates }

let up state = if is_vertical state then
    { state with candidates = Pagination.left state.candidates }
  else
    left state


let right state = 
  if is_vertical state then
    { state with compl_candidates = Pagination.right state.compl_candidates }
  else
    { state with candidates = Pagination.right state.candidates }

let down state = 
  if is_vertical state then
    { state with candidates = Pagination.right state.candidates }
  else
    right state

let remove state =
  if state.before_cursor = "" then
    match List.rev state.entries with
    | [] -> state, false
    | ({ Program.sources ; completion } as program, _) :: rest ->
      on_modify { state with
        before_cursor = ""; after_cursor = ""; program;
        compl_sources = List.map Source.initialize completion;
        sources = List.map Source.initialize sources;
        entries = List.rev rest
      }, true
  else
  on_modify { state with before_cursor = String.rchop state.before_cursor }, false

let get_list state = 
  let s = 
    if Pagination.is_empty state.candidates then
      state.before_cursor ^ state.after_cursor
    else
      (fst (Pagination.selected state.candidates)).real
  in
  List.map (fun (_, s) -> s.real) state.entries @ [s]

let normalize state = 
  { state with
    candidates = Pagination.from_list (split_function state)  (Pagination.all state.candidates) 
  }
