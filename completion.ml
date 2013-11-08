(* File that deal with transforming completion
   sources into a graphical representation *)
open Batteries

type match_result = ((bool * int * int) list) option
(* Candidate and matching *)

type candidate = {
  display: string;
  real: string;
  matching_function: (string -> string -> ((bool * int * int) list) option);
  completion_function: string -> string -> string * string;
}



type 'a source = {
  delay: bool;
  default: 'a;
  compute: 'a -> string -> string -> 'a * candidate list;
}
type source_state = ST : 'a * 'a source -> source_state
type ex_source = S : 'a source -> ex_source
type state = {
  before_cursor: string;
  after_cursor: string;
  sources: (candidate list * source_state) list;
  matches: (candidate * (bool * int * int) list) list;
}

  
let compute_matches before after sources = 
  let candidates = List.concat (List.map fst sources) in
  let matches = List.filter_map (fun candidate ->
    match candidate.matching_function before after with
    | None -> None
    | Some list -> Some (candidate, list)) candidates in
  matches

let on_modify state = 
  let sources = List.map (fun (candidates, ST (sstate, source)) ->
    let new_state, candidates = source.compute sstate state.before_cursor state.after_cursor in
      candidates, ST (new_state, source)) state.sources
  in
  let matches = compute_matches state.before_cursor state.after_cursor sources in
  { state with matches; sources }

let make_state sources = 
  on_modify
    { before_cursor = "";
      after_cursor = "";
      sources = List.map (fun (S s) -> [], ST (s.default, s)) sources;
      matches = [] }
let add_char c state = 
  on_modify { state with before_cursor = state.before_cursor ^ String.of_char c }
  

let remove state = 
  if state.before_cursor = "" then state
  else
    on_modify { state with before_cursor = String.rchop state.before_cursor }
let complete state = 
  let candidate = (fst (List.hd state.matches)) in
  let before, after = candidate.completion_function state.before_cursor state.after_cursor in
  on_modify { state with before_cursor = before; after_cursor = after }

let left state = 
  if state.before_cursor <> "" then
    let c = state.before_cursor.[String.length state.before_cursor-1] in
     on_modify { state with before_cursor = String.rchop state.before_cursor;
       after_cursor = String.of_char c ^ state.after_cursor }
  else
    state
  
let right state = 
  if state.after_cursor <> "" then
    let c = state.after_cursor.[0] in
     on_modify { state with before_cursor = state.before_cursor ^ String.of_char c;
       after_cursor = String.lchop state.after_cursor }
  else
    state
  
