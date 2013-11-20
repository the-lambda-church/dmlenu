open Completion 
open Batteries

(* My Std *)
let ( / ) = Filename.concat

let getenv var =
  try Sys.getenv var
  with Not_found -> ""

(* Word manipulation *)
let rec compute_word_index ?(acc = 0) words position = match words with
  | [] -> acc, 0, "", ""
  | t :: q -> 
    if position <= String.length t then (
      acc, position, String.sub t 0 position,
      String.sub t position (String.length t - position)
    ) else 
      compute_word_index ~acc:(acc + 1) q (position - String.length t - 1)

type word_result = {
  word: string;
  before: string list;
  after: string list;
  index: int;
  index_inside: int;
  before_inside: string;
  after_inside: string;
  new_word : string * string -> string * string;
}

let get_word ?(once = false) sep before after = 
  let total = before ^ after in
  if total = "" then {
    word = ""; before = []; after = []; index = 0; index_inside = 0;
    before_inside = ""; after_inside = ""; new_word = (fun (a, b) -> a, b)
  } else
    let words = 
      if not once then String.nsplit ~by:sep total else
      let (a, b) = try String.split ~by:sep total with _ -> total, "" in
      [a; b]
    in
    let word_index, index_inside, before_inside, after_inside = 
      compute_word_index words (String.length before)
    in
    let word = List.nth words word_index in
    let before, after = List.split_at word_index words in
    let after = List.tl after in
    {
      word; index = word_index; index_inside; before; after;
      before_inside;
      after_inside;
      new_word = begin fun (bef, aft) ->
        String.concat sep (before @ [bef]), String.concat sep (aft :: after)
      end
    }


let complete_in_word ?(drop_cont = false) separator f before after = 
  let w = get_word separator before after in
  let before, after = w.new_word (f w.before_inside w.after_inside) in
  before, if drop_cont then "" else after

let match_in_word separator f before after = 
  let { before_inside ; after_inside ; _ } = get_word separator before after in
  f before_inside after_inside

let match_strict candidate before after = 
  let query = before ^ after in
  try
    let k = String.find candidate query in
    Some [
      (false, 0, k) ;
      (true, k, k + String.length query) ;
      (false, k + String.length query, String.length candidate) ;
    ]
  with _ -> None

type t = ex_source

let dirname s = 
  if s = "" then ""
  else if s.[String.length s -1] = '/' then s
  else Filename.dirname s

let basename s = 
  if s = "" then ""
  else if s.[String.length s -1] = '/' then ""
  else Filename.basename s

(* An example of source *)
let filename =
  let compute (old_dir, cache) before after =
    let directory = getenv "HOME" / dirname before in
    if old_dir = directory && cache <> [] then
      (directory, cache), cache
    else
      let files = try Sys.readdir directory with _ -> [||] in
      let candidates =
        Array.to_list files |>
        List.map (fun file ->
          let real = directory / file in
          let display =
            if Sys.file_exists real && Sys.is_directory real then 
              file ^ "/"
            else file
          in
          let completion_function =
            complete_in_word ~drop_cont:true "/" (fun _ _ -> display, "")
          in
          let matching_function =
            match_in_word "/" (fun bef aft ->
                match_strict display (basename (bef^aft)) "")
          in
          { display ; real ; completion_function ; matching_function }
        )
      in
      (directory, candidates), candidates
  in
  S { delay = false ; default = (getenv "HOME", []) ; compute }

let from_list list = 
  let candidates =
    let aux (display, real) = {
      display; real; matching_function = match_strict display;
      completion_function = (fun _ _ -> display, "")
    }
    in
    List.map aux list
  in
  S { delay = false; default = (); compute = (fun () _ _ -> (), candidates) }

let reindex_candidates sep = 
  let reindex_one c = { c with
    matching_function   = match_in_word sep c.matching_function ;
    completion_function = complete_in_word sep c.completion_function  ;
  }
  in
  List.map reindex_one

let kleene sep (S source) = 
  let compute states before after =
    let { index ; before_inside ; after_inside } = get_word sep before after in
    let state =
      try List.assoc index states
      with Not_found -> source.default
    in
    let new_state, lst = source.compute state before_inside after_inside in
    let candidates = reindex_candidates sep lst in
    (index, new_state) :: List.filter (fun (k, _) -> k <> index) states, candidates
  in
  S { delay = false ; default = [] ; compute }

let concat sep (S source) (S source') =
  let compute (s1, s2) before after =
    let {index; before_inside; after_inside} = get_word ~once:true sep before after in
    match index with
    | 0 ->
      let s1', candidates = source.compute s1 before_inside after_inside in
      (s1', s2), reindex_candidates sep candidates
    | 1 ->
      let s2', candidates = source'.compute s2 before_inside after_inside in
      (s1, s2'), reindex_candidates sep candidates
    | _ -> (s1, s2), []
  in
  S { delay = false; default = (source.default, source'.default) ; compute }
      
let binaries = 
  let aux s =
    try Array.to_list (Sys.readdir s) |> List.map (fun s' -> s', s / s')
    with _ -> []
  in
  String.nsplit ~by:":" (getenv "PATH") |> List.map aux |> List.concat |> from_list
  
