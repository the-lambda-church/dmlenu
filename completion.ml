(* File that deal with transforming completion
   sources into a graphical representation *)
open Batteries

type candidate = {
  display: string;
  real: string;
  matching_function: (string -> (bool * int * int) list option);
}

type input = {
  before : string list;
  after: string list;
  current: string;
}
type source = {
  cache: bool;
  delay: bool;
  compute: input -> candidate list;
}

let match_strict candidate query = 
  try
    let k = String.find candidate query in
    Some [(false, 0, k); 
          (true, k, k + String.length query); 
          (false, (k+String.length query), String.length candidate)]
  with _ -> None

(* An example of source *)
let filename = {
  cache = true;
  delay = false;
  compute = (fun input ->
    let ( ^^ ) = Filename.concat in
    let directory = Sys.getenv "HOME" ^^ String.concat "/" input.before in
    let files = Sys.readdir directory in
    Printf.eprintf "%s / %s\n" directory input.current;
    Array.to_list files |>
        List.map (fun file ->
          let real = directory ^^ file in
          let display = if Sys.file_exists real && Sys.is_directory real then 
              file ^ "/"
            else file in
          { display; real;
            matching_function = match_strict display  })
    )
}
type cmatch = candidate * (bool * int * int) list
type completion_state = {
  before_cursor: string;
  after_cursor: string;
  sources: (candidate list * source) list;
  matches: cmatch list;
  separator: string;
}

let rec compute_word_index ?(acc = 0) words position = match words with
  | [] -> raise Not_found
  | t :: q -> 
    if position <= String.length t then acc
    else compute_word_index ~acc:(acc + 1) q (position - String.length t - 1)

let get_word state = 
  let total = state.before_cursor ^ state.after_cursor in
  let words = String.nsplit ~by:state.separator total in
  let word_index = compute_word_index words (String.length state.before_cursor) in
  let word = List.nth words word_index in
  let before, after = List.split_at word_index words in
  let after = List.tl after in
  before, after, word, fun new_word ->
    String.concat state.separator (before @ [new_word]), 
    String.concat state.separator after
  
let compute_matches word sources = 
  let candidates = List.concat (List.map fst sources) in
  let matches = List.filter_map (fun candidate ->
    match candidate.matching_function word with
    | None -> None
    | Some list -> Some (candidate, list)) candidates in
  matches

let on_modify state = 
  let before, after, current, _ = get_word state in
  let sources = List.map (fun (candidate, source) ->
    Printf.eprintf "Computing match for %s. Before / After / Current %s, %s, %s\n" (state.before_cursor ^ state.after_cursor)(String.concat state.separator before)
      current (String.concat state.separator after);
    if not source.cache && candidate <> [] then (candidate, source)
    else source.compute {before; after; current}, source) state.sources
  in
  let matches = compute_matches current sources in
  { state with matches; sources }

let add_char c state = 
  on_modify { state with before_cursor = state.before_cursor ^ String.of_char c }
  

let remove state = 
  if state.before_cursor = "" then state
  else
    on_modify { state with before_cursor = String.rchop state.before_cursor }
let complete state = 
  let candidate = (fst (List.hd state.matches)).display in
  let _, _, word, on_new_word = get_word state in
  let before, after = on_new_word candidate in
  on_modify { state with before_cursor = before; after_cursor = after }

let test_state = {
  sources = [[], filename];
  before_cursor = "Ennio ";
  after_cursor = "";
  matches = [];
  separator = "/"
}
  
