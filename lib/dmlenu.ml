open Batteries
open Candidate


type app_state = {
  colors: X.Colors.t; (** The set of colors to use *)
  prompt: string; (** The prompt to the user *)
  topbar: bool;  (** Shall dmlenu sit on the bottom or on the top of the screen? *)
  hook: (app_state -> app_state); (** Hook called whenever a token is added *)
  state: State.t; (** The state of the current engine *)
  xstate: X.state;
}

let splith state list =
  let rec go index = function
    | [] -> [], []
    | (candidate, rest) :: q as l->
      let size = X.text_width ~state (candidate.display) + 10 in
      if (index + size) > X.width ~state - 5 then [], l else
      let a, b = go (index+size) q in
      (candidate, rest) :: a, b
  in
  go 0 list

let incr ~xstate = X.Draw.update_x ~state: xstate ((+) 5)

let draw_horizontal { xstate; state } = 
  let open State in
  let candidates = state.candidates in
  if candidates.Pagination.unvisible_left <> [] then begin 
    X.Draw.text ~state: xstate ~focus: false "<";
    incr ~xstate
  end ;
  
  Pagination.fold_visible (fun () visible (candidate, result) ->
    X.Draw.text_hl ~focus: visible ~result ~state: xstate candidate.display;
    X.Draw.update_x ~state: xstate ((+) 5)
  ) () candidates ;

  if candidates.Pagination.unvisible_right <> [] then begin
    incr ~xstate ;
    X.Draw.text ~state: xstate ~focus: false ">"
  end

let rec shorten ~state candidate s = 
  if X.text_width ~state (candidate.display ^ s) <= X.width ~state - 10 then 
    s
  else
    shorten ~state candidate
      (try "..." ^ String.sub s 10 (String.length s - 10) with _ -> "")

let draw_vertical xstate topbar candidates =
  let init, f = 
    if topbar then 1, succ else
    List.length candidates.Pagination.visible, pred
  in
  Pagination.fold_visible (fun line focus (candidate, result) ->
    X.Draw.set_x ~state: xstate ~x: 0;
    X.Draw.set_line ~state: xstate ~line;
    X.Draw.text_hl ~state: xstate ~focus ~result candidate.display;
    if candidate.doc <> "" then (
      let str = shorten ~state: xstate candidate candidate.doc in
      let x = X.width ~state: xstate - (X.text_width ~state: xstate str) - 10 in
      let () = X.Draw.set_x ~state: xstate ~x in
      X.Draw.text ~focus: false ~state: xstate "%s" candidate.doc
    ) ;
    f line
  ) init candidates
  |> ignore

let resize =
  let current = ref 0 in
  fun ~state ~lines ->
    if !current <> lines then (
      current := lines ;
      X.resize ~state ~lines
    )

let draw ({ xstate; prompt; state } as app_state) = 
  let open State in
  let lines =
    match state.layout with
    | SingleLine | Grid (_, None) -> 0
    | Grid (n, Some { pages }) -> min n (List.length pages.Pagination.visible)
    | MultiLine n ->
      min n (List.length state.State.candidates.Pagination.visible)
  in
  resize ~state:xstate ~lines ;
  X.Draw.clear ~state:xstate;

  X.Draw.text ~state: xstate ~focus: true "%s" prompt;
  incr ~xstate;

  state.State.entries |> List.iter (fun (_, candidate) -> 
    X.Draw.text ~state: xstate ~focus: false "%s" candidate.display;
    incr ~xstate
  ) ;

  State.(
    X.Draw.text ~state:xstate ~focus:false
      "%s|%s" state.before_cursor state.after_cursor
  ) ;

  incr ~xstate;

  begin match app_state.state.State.layout with
  | State.Grid (_, None)
  | State.SingleLine  -> draw_horizontal app_state

  | State.MultiLine _ ->
    draw_vertical app_state.xstate app_state.topbar
      app_state.state.State.candidates

  | State.Grid (_, Some { pages ; _ }) ->
    draw_horizontal app_state ;
    draw_vertical app_state.xstate app_state.topbar pages
  end ;

  X.Draw.map ~state:xstate


let run_list ?(topbar = true) ?(separator = " ") ?(colors = X.Colors.default) 
    ?(layout = State.SingleLine) ?(prompt = "") ?(hook = fun x -> x) program = 
  let hook state = 
    let state = hook state in
    { state with state = State.normalize state.state }
  in
  match X.setup ~topbar ~colors ~lines:(State.nb_lines layout) with
  | None -> failwith "X.setup"
  | Some xstate -> 
    let state = {
      colors; prompt; topbar; hook; xstate;
      state = State.initial ~layout ~separator ~program ~split:(splith xstate)
    } in
    let rec loop state = 
      let loop_pure f = loop { state with state = f state.state } in
      let loop_transition f = 
        let state', b = f state.state in
        let state' = { state with state = state' } in
        loop (if b then state'.hook state' else state')
      in
      let open X.Events in
      draw state;
      match X.Events.poll ~state: xstate ~timeout: 1. with
      | Some (Key (0xff1b, _)) -> X.quit xstate; []

      (* Arrows *)
      | Some (Key (0xff51, _)) -> loop_pure State.left
      | Some (Key (0xff52, _)) -> loop_pure State.up
      | Some (Key (0xff53, _)) -> loop_pure State.right
      | Some (Key (0xff54, _)) -> loop_pure State.down

      (* Enter *)
      | Some (Key (0xff0d, _)) -> 
        X.quit xstate; State.get_list state.state

      (* Tab *)
      | Some (Key (0xff09, _)) -> loop_transition State.complete
      (* Backspace *)
      | Some (Key (0xff08, _)) -> loop_transition State.remove

      | Some (Key (_, s)) -> loop_transition (State.add_char s)

      | _ -> loop state
    in
    try loop state with e -> 
      X.quit ~state: state.xstate; 
      raise e

let run ?topbar ?separator ?colors ?layout ?prompt ?hook program = 
  match
    (run_list ?topbar ?separator ?colors ?layout ?prompt ?hook program)
  with
  | [] -> None
  | l -> Some (String.concat (Option.default " " separator) l)
