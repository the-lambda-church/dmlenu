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

let match_in_word separator f ~query = 
  let { word ; _ } = get_word separator query "" in
  f word

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
  else match Filename.dirname s with
  | "." -> ""
  | s -> s

let basename s = 
  if s = "" then ""
  else if s.[String.length s -1] = '/' then ""
  else Filename.basename s

let expand_tilde s = try
  Str.global_replace (Str.regexp "^~") (getenv "HOME") s
  with _ -> s
let filename root =
  let root = root / "" in (* make it end by a slash *)
  let compute (old_dir, cache) _ query =
    let query = expand_tilde query in
    let directory = if query <> "" && query.[0] = '/' then
        dirname query
      else
        root / dirname query 
    in
    if old_dir = directory && cache <> [] then
      (directory, cache), cache
    else
      let files = try Sys.readdir directory with _ -> [||] in
      let candidates =
        Array.to_list files |>
        List.map (fun file ->
          let real = directory / file in
          let real, display =
            if Sys.file_exists real && Sys.is_directory real then 
              real ^ "/", file ^ "/"
            else real, file
          in
          let matching_function =
            match_in_word "/" (fun query ->
                Matching.match_query ~case: false ~query: (basename query) 
                  ~candidate: display)
          in
          let completion = real in
          { display ; 
            completion; documentation = lazy [];
            real ; matching_function }
        )
      in
      (directory, candidates), candidates
  in
  S { delay = false ; default = (getenv "HOME", []) ; compute }

let from_list list = 
  let candidates =
    let aux (display, real) = {
      display; real; completion = display; documentation = lazy [];
      matching_function = Matching.match_query ~case: true ~candidate: display;
    }
    in
    List.map aux list
  in
  S { delay = false; default = (); compute = (fun () _ _ -> (), candidates) }

let from_list_ list = List.map (fun x -> x, x) list |> from_list
let stdin ?sep () = 
  IO.lines_of stdin |> Enum.map (fun s ->
    match sep with
    | None -> s, s
    | Some c -> try String.split s ~by: c with _ -> s, s) |> List.of_enum |> from_list
      
let binaries = 
  let aux s =
    let helper s' =
      let full_path = s / s' in
      try
        let { Unix. st_perm ; _ } = Unix.stat full_path in
        if st_perm land 1 = 1 then Some (s', full_path) else None
      with Unix.Unix_error (Unix.ENOENT, "stat", _) ->
        (* File doesn't exist... Broken link? *)
        None
    in
    try Array.to_list (Sys.readdir s) |> List.filter_map helper
    with _ -> []
  in
  let lower_compare s1 s2 = String.(compare (lowercase s1) (lowercase s2)) in
  String.nsplit ~by:":" (getenv "PATH") |> List.map aux |> List.concat
  |> List.sort (fun (s1, _) (s2, _) -> lower_compare s1 s2)
  |> from_list

let empty = S {
  delay = false;
  default = ();
  compute = fun _ _ _ -> (), []
}

let switch list = 
  S {
    delay = false;
    default = None;
    compute = fun st entries query ->
      let (S source) = snd (List.find (fun (f, b) -> f query) list) in
      match st with
      | Some (ST (state, source')) when Obj.magic source' == Obj.magic source ->
        let state, answer = source'.compute state entries query in
        Some (ST (state, source')), answer
      | _ -> 
        let state, answer = source.compute source.default entries query in
        Some (ST (state, source)), answer
  }

let paths ~coupled_with = 
  switch [
    flip String.starts_with "./", filename (Sys.getcwd ());
    flip String.starts_with "~/", filename (getenv "HOME");
    flip String.starts_with "/", filename "/";
    (fun _ -> true), coupled_with
  ]

let subcommands : (string * t Lazy.t) list ref = ref []
let default_subcommand_hook : (string -> t) ref =
  ref (
    fun source_name ->
      let prefix = Sys.getenv "HOME" / ".config/dmlenu" in
      let file = prefix / source_name  in
      if Sys.file_exists file then
        from_list_ (File.lines_of file |> List.of_enum)
      else
        paths ~coupled_with:binaries
  )


let set_default_subcommand_hook fn = default_subcommand_hook := fn
let add_subcommand ~name lazy_source =
  subcommands := (name, lazy_source) :: !subcommands

let get_subcommand_by_name name =
  try Some (List.assoc name !subcommands)
  with Not_found -> None

let binaries_with_subcommands =
  let initial = paths ~coupled_with:binaries in
  let rec get_new_source source_name =
    let new_src =
      match get_subcommand_by_name source_name with
      | None -> !default_subcommand_hook source_name
      | Some src -> Lazy.force src
    in
    Program ([new_src], (fun _ x -> get_new_source (source_name ^ x)))
  in
  Program ([initial], fun _ -> get_new_source)
