(* File that deal with transforming completion
   sources into a graphical representation *)
open Batteries

type candidate = {
  display: string;
  real: string;
  matching_function: (query: string -> Matching.result option);
}



type 'a source = {
  delay: bool;
  default: 'a;
  compute: 'a -> string -> 'a * candidate list;
}
type source_state = ST : 'a * 'a source -> source_state
type ex_source = S : 'a source -> ex_source
type program = Program of ex_source list * (string -> string -> program)
type state = {
  before_cursor: string;
  after_cursor: string;
  sources: (candidate list * source_state) list;
  before_matches: (candidate * Matching.result) list;
  after_matches: (candidate * Matching.result) list;
  entries: (program * string * string) list;
  program: program;
}

let rec empty_program = Program ([], fun _ _ -> empty_program)

let compute_matches before after sources = 
  let aux candidate =
    match candidate.matching_function ~query: (before ^ after) with
    | None -> None
    | Some list -> Some (candidate, list)
  in
  List.filter_map aux (List.concat (List.map fst sources))

let on_modify st = 
  let sources =
    List.map (fun (candidates, ST (sstate, source)) ->
        let new_state, candidates =
          source.compute sstate (st.before_cursor ^ st.after_cursor)
        in
        candidates, ST (new_state, source)
      ) st.sources
  in
  let after_matches = compute_matches st.before_cursor st.after_cursor sources in
  { st with before_matches = []; after_matches ; sources }

let make_state (Program (sources, _) as program) = 
  on_modify {
    before_cursor = "";
    after_cursor = "";
    sources = List.map (fun (S s) -> [], ST (s.default, s)) sources;
    program;
    after_matches = []; before_matches = [];
    entries = []
  }

  

let remove state = 
  if state.before_cursor = "" then 
    match List.rev state.entries with
    | [] -> state
    | (program, _, _) :: rest -> 
      on_modify { state with 
        before_cursor = ""; after_cursor = ""; program;
        entries = rest
      }
  else
  on_modify { state with before_cursor = String.rchop state.before_cursor }

let complete state = 
  try
    let candidate = (fst (List.hd state.after_matches)) in
    let (Program (_, f)) = state.program in
    let (Program (sources, _) as program) = f candidate.real candidate.display in
    on_modify { 
      before_cursor = "" ; 
      after_cursor = ""; 
      program;
      after_matches = []; before_matches = [];
      sources = List.map (fun (S x) -> [], ST (x.default, x)) sources;
      entries = state.entries @ [state.program, candidate.real, candidate.display]
    }
  with Failure "hd" ->
    state

let add_string s state = 
  let state' = on_modify { state with before_cursor = state.before_cursor ^ s } in
  if state'.after_matches = [] && state.after_matches <> [] && s = " " then
    complete state
  else
    state'

let cursor_left state = 
  if state.before_cursor = "" then state else
  let c = state.before_cursor.[String.length state.before_cursor - 1] in
  on_modify { state with
    before_cursor = String.rchop state.before_cursor;
    after_cursor  = String.of_char c ^ state.after_cursor;
  }
  
let cursor_right state = 
  if state.after_cursor = "" then state else
  let c = state.after_cursor.[0] in
  on_modify { state with
    before_cursor = state.before_cursor ^ String.of_char c;
    after_cursor  = String.lchop state.after_cursor;
  }

let left state = 
  match state.before_matches with
  | [] -> 
    state
  | t :: before_matches -> 
    { state with before_matches; after_matches = t :: state.after_matches }

let right state = 
  match state.after_matches with
  | [] -> 
    state
  | t :: after_matches -> 
    { state with after_matches; before_matches = t :: state.before_matches }
  

let pageup f state = 
  let visible, invisible = f state.before_matches in
  { state with 
    before_matches = invisible;
    after_matches = List.rev visible @ state.after_matches
  }

let pagedown f state = 
  let visible, invisible = f state.after_matches in
  { state with 
    after_matches = invisible;
    before_matches = List.rev visible @ state.before_matches
  }
    
