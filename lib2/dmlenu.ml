open Batteries


type app_state = {
  colors: X.Colors.t; (** The set of colors to use *)
  lines: int; (** The maximum number of lines to use. (0 means no lines) *)
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
      let size = X.text_width ~state (candidate#display) + 10 in
      if (index + size) > X.width ~state - 5 then [], l else
      let a, b = go (index+size) q in
      (candidate, rest) :: a, b
  in
  go 0 list

let incr ~xstate = X.Draw.update_x ~state: xstate ((+) 5)

let draw_horizontal { xstate; lines; state } = 
  let candidates = 
    let open State in
    if lines > 0 then state.compl_candidates
    else state.candidates
  in

  if candidates.Pagination.unvisible_left <> [] then
      begin 
        X.Draw.text ~state: xstate ~focus: false "<";
        incr ~xstate
      end;
  
  Pagination.fold_visible (fun () visible (candidate, result) ->
    X.Draw.text_hl ~focus: visible ~result ~state: xstate candidate#display;
    X.Draw.update_x ~state: xstate ((+) 5))
    () candidates;

  if candidates.Pagination.unvisible_right <> [] then
    begin
      incr ~xstate;
      X.Draw.text ~state: xstate ~focus: false ">"
    end

let rec shorten ~state candidate s = 
  if X.text_width ~state (candidate#display ^ s) <= X.width ~state - 10 then 
    s
  else
    shorten ~state candidate
      (try "..." ^ String.sub s 10 (String.length s - 10) with _ -> "")

let draw_vertical { xstate; state = { candidates } } =
  Pagination.fold_visible (fun line focus (candidate, result) ->
    X.Draw.set_x ~state: xstate ~x: 0;
    X.Draw.set_line ~state: xstate ~line;
    X.Draw.text_hl ~state: xstate ~focus ~result candidate#display;
    print_endline candidate#doc;
    if candidate#doc <> "" then
      (let str = shorten ~state: xstate candidate candidate#doc in
      let x = X.width ~state: xstate - (X.text_width ~state: xstate str) - 10 in
      let () = X.Draw.set_x ~state: xstate ~x in
        X.Draw.text ~focus: false ~state: xstate "%s" candidate#doc);
    line + 1)
    1 candidates |> ignore
          




let draw ({ xstate; prompt; state } as app_state) = 
  X.Draw.clear ~state: xstate;

  X.Draw.text ~state: xstate ~focus: true "%s" prompt;
  incr ~xstate;

  State.(X.Draw.text 
           ~state: xstate 
           ~focus: false
           "%s|%s" state.before_cursor state.after_cursor);
  incr ~xstate;

  draw_horizontal app_state;

  if app_state.lines > 0 then
    draw_vertical app_state;

  X.Draw.map ~state: xstate


let run_list ?(topbar = true) ?(separator = " ") ?(colors = X.Colors.default) 
    ?(lines = 0) ?(prompt = "") ?(hook = fun x -> x) program = 
  match X.setup ~topbar ~colors ~lines with
  | None -> failwith "X.setup"
  | Some xstate -> 
    let rec splitv k l = 
      if List.length l <= k then l, [] else List.split_at k l 
    in
    let state = {
      colors; lines; prompt; topbar; hook; xstate;
      state = State.initial ~separator ~program ~splitc: (splith xstate) 
        ~splitm: (if lines > 0 then splitv lines
                  else splith xstate)
    } in
    let rec loop state = 
      let loop_pure f = loop { state with state = f state.state } in
      let open X.Events in
      draw state;
      match X.Events.poll ~state: xstate ~timeout: 1. with
      | Some (Key (0xff1b, _)) -> X.quit xstate; []
      | Some (Key ((0xff51 | 0xff52), _)) -> loop_pure State.left
      | Some (Key ((0xff53 | 0xff54), _)) -> loop_pure State.right
      | Some (Key (_, s)) -> loop_pure (State.add_char s)
      | _ -> loop state
    in
    try loop state with e -> 
      X.quit ~state: state.xstate; 
      raise e

