type app_state = {
  colors: X.Colors.t; (** The set of colors to use *)
  lines: int; (** The maximum number of lines to use. (0 means no lines) *)
  prompt: string; (** The prompt to the user *)
  topbar: bool;  (** Shall dmlenu sit on the bottom or on the top of the screen? *)
  hook: (app_state -> app_state); (** Hook called whenever a token is added *)
  state: State.t; (** The state of the current engine *)
  xstate: X.state;
}

let draw { xstate; prompt } = 
  X.Draw.clear ~state: xstate;
  X.Draw.text ~state: xstate ~focus: true "%s" prompt;
  X.Draw.map ~state: xstate

let run_list ?(topbar = true) ?(separator = " ") ?(colors = X.Colors.default) 
    ?(lines = 0) ?(prompt = "") ?(hook = fun x -> x) program = 
  match X.setup ~topbar ~colors ~lines with
  | None -> failwith "X.setup"
  | Some xstate -> 
    let splith l = l, [] in
    let state = {
      colors; lines; prompt; topbar; hook; xstate;
      state = State.initial ~separator ~program ~splith
    } in
    let rec loop state = 
      draw state;
      match X.Events.poll ~state: xstate ~timeout: 1. with
      | Some (Key (0xff1b, _)) -> X.quit xstate; []
      | _ -> loop state
    in
    loop state

