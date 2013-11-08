(* File that deal with transforming completion
   sources into a graphical representation *)
open Batteries

type candidate = {
  display: string;
  real: string;
  matching_function: (string -> string -> ((bool * int * int) list) option);
  completion_function: string -> string -> string * string;
}

type source = {
  cache: bool;
  delay: bool;
  compute: string -> string -> candidate list;
}

let match_strict candidate query = 
  try
    let k = String.find candidate query in
    Some [(false, 0, k); 
          (true, k, k + String.length query); 
          (false, (k+String.length query), String.length candidate)]
  with _ -> None

let rec compute_word_index ?(acc = 0) words position = match words with
  | [] -> acc
  | t :: q -> 
    if position <= String.length t then acc
    else compute_word_index ~acc:(acc + 1) q (position - String.length t - 1)

let get_word sep before after = 
  let total = before ^ after in
  let words = String.nsplit ~by:sep total in
  let word_index = compute_word_index words (String.length before) in
  let word = List.nth words word_index in
  let before, after = List.split_at word_index words in
  let after = List.tl after in
  before, after, word, fun new_word ->
    String.concat sep (before @ [new_word]), 
    String.concat sep after

let complete_in_word ?(drop_cont = false) separator f before after = 
  let before, after, word, new_word = get_word separator before after in
  let before, after = new_word (f word) in
  if drop_cont then before, ""
  else before, after
let match_in_word separator f before after = 
  let before, after, word, new_word = get_word separator before after in
  f word
let dirname s = 
  if s = "" then "" else 
    if s.[String.length s -1] = '/' then s else Filename.dirname s
let basename s = 
  if s = "" then "" else
    if s.[String.length s -1] = '/' then "" else Filename.basename s
(* An example of source *)
let filename = {
  cache = true;
  delay = false;
  compute = (fun before after ->
    let ( ^^ ) = Filename.concat in
    let directory = Sys.getenv "HOME" ^^ dirname before in
    let files = Sys.readdir directory in
    print_endline (">"^directory);
    Array.to_list files |>
        List.map (fun file ->
          let real = directory ^^ file in
          let display = if Sys.file_exists real && Sys.is_directory real then 
              file ^ "/"
            else file in
          { display; real; 
            completion_function = complete_in_word ~drop_cont: true "/" (fun _ -> display);
            matching_function = (match_in_word "/" 
                                   (fun s -> match_strict display (basename s))) })
    )
}

type cmatch = candidate * (bool * int * int) list
type completion_state = {
  before_cursor: string;
  after_cursor: string;
  sources: (candidate list * source) list;
  matches: cmatch list;
}

  
let compute_matches before after sources = 
  let candidates = List.concat (List.map fst sources) in
  let matches = List.filter_map (fun candidate ->
    match candidate.matching_function before after with
    | None -> None
    | Some list -> Some (candidate, list)) candidates in
  matches

let on_modify state = 
  let sources = List.map (fun (candidate, source) ->
    if not source.cache && candidate <> [] then (candidate, source)
    else source.compute state.before_cursor state.after_cursor, source) state.sources
  in
  let matches = compute_matches state.before_cursor state.after_cursor sources in
  { state with matches; sources }

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
  
