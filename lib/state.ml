open Batteries
open Candidate

type column = {
  src : Candidate.t ; (* used for caching *)
  pages : (Candidate.t * Matching.result) Pagination.t ;
}

type layout =
  | SingleLine
  | MultiLine of int
  | Grid of int * column option

type t = {
  separator: string;
  before_cursor: string;
  after_cursor : string;
  program: Engine.t;
  sources : Source.state list;
  candidates: (Candidate.t * Matching.result) Pagination.t;
  entries: (Engine.t * Candidate.t) list;
  split: (Candidate.t * Matching.result) list -> (Candidate.t * Matching.result) list * (Candidate.t * Matching.result) list;
  layout : layout ;
}

let nb_lines = function
  | SingleLine  | Grid (_, None) -> 0
  | MultiLine n | Grid (n, _)    -> n

let take lines a =
  if List.length a < lines then a, []
  else List.split_at lines a

let split_function state =
  match state.layout with
  | MultiLine n -> take n
  | _ -> state.split


let update_sources ?(input="") split sources =
  let r = ref [] in
  let sources = sources |> List.map (fun (Source.ST (state, s)) ->
    let state', candidates = s.Source.compute state input in
    let test c = Option.map (fun x -> c, x) (c.matching_function input) in
    r := !r @ List.filter_map test candidates; (* !r is empty most of the times *)
    Source.ST (state', s))
  in
  sources, Pagination.from_list split !r

let update_layout state candidates =
  match state.layout with
  | Grid (n, column) ->
    begin try
      let candidate, _ = Pagination.selected candidates in
      match column with
      | Some c when c.src.display = candidate.display -> state.layout
      | _ ->
        let next_program = state.program.Engine.transition candidate in
        let next_sources = List.map Source.initialize next_program.Engine.sources in
        let _, pages = update_sources (take n) next_sources in
        Grid (n, Some { src = candidate ; pages = pages })
    with
    | Failure _ -> Grid (n, None)
    end
  | layout -> layout

let on_modify state =
  let input = state.before_cursor ^ state.after_cursor in
  let sources, candidates =
    update_sources ~input (split_function state) state.sources
  in
  let layout = update_layout state candidates in
  { state with sources ; candidates ; layout }


let initial ~separator ~program ~layout ~split =
  let lines = nb_lines layout in
  on_modify {
    split; layout; separator; program;
    before_cursor = ""; after_cursor = "";
    entries = [];
    sources = List.map Source.initialize program.Engine.sources;
    candidates = Pagination.from_list (if lines > 0 then take lines else split) [];
  }

let next_entry candidate state =
  let f = state.program.Engine.transition in
  let ({ Engine.sources ; _ } as program) = f candidate in
  on_modify { state with
    before_cursor = "";
    after_cursor = "";
    sources = List.map Source.initialize sources;
    candidates = Pagination.from_list (split_function state) [];
    separator = state.separator;
    program;
    entries = state.entries @ [state.program, candidate]
  }

let complete state =
  try
    let candidate = fst (Pagination.selected state.candidates) in
    let state' = on_modify {
      state with before_cursor = candidate.completion; after_cursor = ""
    } in
    if Pagination.all state'.candidates |>
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
    let first = fst @@ Pagination.selected state.candidates in
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
  let candidates = Pagination.left state.candidates in
  match state.layout with
  | Grid _ ->
    let layout = update_layout state candidates in
    { state with candidates ; layout }
  | _ -> { state with candidates }

let up state =
  match state.layout with
  | Grid (_, None) -> state
  | Grid (n, Some column) ->
    let column' = { column with pages = Pagination.left column.pages } in
    { state with layout = Grid (n, Some column') }
  | _ -> { state with candidates = Pagination.left state.candidates }

let scroll_up state =
  match state.layout with
  | Grid (_, None) -> state
  | Grid (n, Some column) ->
    let column' = { column with pages = Pagination.page_left column.pages } in
    { state with layout = Grid (n, Some column') }
  | _ -> { state with candidates = Pagination.page_left state.candidates }

let right state =
  let candidates = Pagination.right state.candidates in
  match state.layout with
  | Grid _ ->
    let layout = update_layout state candidates in
    { state with candidates ; layout }
  | _ -> { state with candidates }

let down state =
  match state.layout with
  | Grid (_, None) -> state
  | Grid (n, Some column) ->
    let column' = { column with pages = Pagination.right column.pages } in
    { state with layout = Grid (n, Some column') }
  | _ -> { state with candidates = Pagination.right state.candidates }

let scroll_down state =
  match state.layout with
  | Grid (_, None) -> state
  | Grid (n, Some column) ->
    let column' = { column with pages = Pagination.page_right column.pages } in
    { state with layout = Grid (n, Some column') }
  | _ -> { state with candidates = Pagination.page_right state.candidates }

let remove state =
  if state.before_cursor = "" then
    match List.rev state.entries with
    | [] -> state, false
    | ({ Engine.sources ; _ } as program, _) :: rest ->
      on_modify { state with
        before_cursor = ""; after_cursor = ""; program;
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
