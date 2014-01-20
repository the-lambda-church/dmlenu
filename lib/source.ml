open Batteries
type 'a t_open = {
  delay: bool;
  default_state: 'a;
  compute: 'a -> string -> ('a * Candidate.t list);
}

type t = S : 'a t_open  -> t

(* Useful functions *)
let (/) = Filename.concat

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

let match_in_word separator f query =
  let { word ; _ } = get_word separator query "" in
  f word

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

(* Actual sources *)
let files ?(filter=fun x -> true) root =
  let root = root / "" in (* make it end by a slash *)
  let compute (old_dir, cache) query =
    let query = expand_tilde query in
    let directory = if query <> "" && query.[0] = '/' then
        dirname query
      else
        root / dirname query
    in
    if old_dir = directory && cache <> [] then
      ((directory, cache), cache)
    else
      let files = try Sys.readdir directory with _ -> [||] in
      let candidates =
        Array.to_list files |>
        List.filter_map (fun file ->
          let abs_path = directory / file in
          if not (filter abs_path) then None else
          let real, display =
            if Sys.file_exists abs_path && Sys.is_directory abs_path then
              abs_path ^ "/", file ^ "/"
            else
              abs_path, file
          in
          let matching_function =
            match_in_word "/" (fun query ->
              Matching.match_query ~candidate:display (basename query)
            )
          in
          Some Candidate.(
            make ~completion:real ~real ~doc:abs_path
              ~matching_function display
          )
        )
      in
      ((directory, candidates), candidates)
  in
  S { delay = false ; default_state = (root, []) ; compute }

let from_list_aux (display, real, doc) =
  Candidate.make ~real ~doc display

let from_list_rev list =
  let candidates = List.rev_map from_list_aux list in
  S { delay = false; default_state = (); 
      compute = (fun () _ -> ((), candidates)) }

let from_list_lazy list = 
  let candidates = lazy (List.map from_list_aux (Lazy.force list)) in
  S { delay = false; default_state = ();
      compute = (fun () _ -> ((), Lazy.force candidates)) }

let from_list_lazy_ list = 
  from_list_lazy (lazy (List.map (fun s -> s, s, "") (Lazy.force list)))

let from_list list = from_list_lazy (lazy list)
let from_list_ list = from_list_lazy_ (lazy list)
let from_list_rev_ list = List.rev_map (fun x -> x, x, "") list |> from_list


let stdin ?sep () =
  IO.lines_of stdin |> Enum.map (fun s ->
    match sep with
    | None -> s, s, ""
    | Some c ->
      try
        let display, real = String.split s ~by:c in
        display, real, ""
      with _ -> s, s, ""
  ) |> List.of_enum |> from_list

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
  |> List.map (fun (x, y) -> (x, y, ""))
  |> from_list

let empty = S {
  delay = false;
  default_state = ();
  compute = fun _ _ -> ((), [])
}


type state = ST : 'a * 'a t_open -> state
let switch list =
  S {
    delay = false;
    default_state = None;
    compute = fun st query ->
      let (S source) =
        Lazy.force @@ snd (List.find (fun (f, b) -> f query) list)
      in
      match st with
      | Some (ST (state, source')) when Obj.magic source' == Obj.magic source ->
        let (state, answer) = source'.compute state query in
        Some (ST (state, source')), answer
      | _ ->
        let (state, answer) = source.compute source.default_state query in
        Some (ST (state, source)), answer
  }

let paths ~coupled_with =
  switch [
    flip String.starts_with "./", lazy (files (Sys.getcwd ()));
    flip String.starts_with "~/", lazy (files (getenv "HOME"));
    flip String.starts_with "/",  lazy (files "/");
    (fun _ -> true), Lazy.from_val coupled_with
  ]

let initialize (S x) = ST (x.default_state, x)

let update_matching f (S x) = 
  let open Candidate in
  S { x
      with compute = (fun state query ->
        let state', candidates = x.compute state query in
        state', candidates |> List.map 
            (fun c -> { c with matching_function = f c.matching_function }))
    }
        
          
