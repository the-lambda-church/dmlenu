open Batteries
open Completion

type conf = {
  stdin: bool;
  bottom: bool;
  focus_foreground: string;
  focus_background: string;
  normal_foreground: string;
  normal_background: string;
  match_foreground: string;
  lines: int;
  window_background: string;
}

let default_conf = {
  stdin = false ;
  bottom = false ;
  normal_background = "#222222" ;
  normal_foreground = "#bbbbbb" ;
  focus_background = "#005577" ;
  focus_foreground = "#eeeeee" ;
  match_foreground = "#ff0000" ;
  lines = 0 ;
  window_background = "#000000" ;
}


let draw_match ?(hl = false) conf x (candidate, list) =
  10 + Draw.draw_text candidate.display x list
    (conf.normal_foreground, conf.match_foreground, 
     if hl then conf.focus_background else conf.normal_background)
  
type app_state = {
  compl: state;
  prompt: string;
}

let displayable_matches x = 
  let rec go index = function
    | [] -> [], []
    | ({display} as candidate, rest) :: q as l-> 
      let size = Draw.size (display) + 10 in
      if (index + size) > Draw.width () then [], l else
      let a, b = go (index+size) q in
      (candidate, rest) :: a, b
  in
  go x

let draw_matches conf state =
  let x =
    if state.prompt = "" then 0 else
    Draw.(text ~x:0 ~fg:conf.focus_foreground ~bg:conf.focus_background "%s" state.prompt)
  in
  let x = List.fold_left Draw.(fun x (_, _, display) ->
    2+text ~x ~fg: conf.focus_foreground ~bg:conf.focus_background "%s" display) x state.compl.entries
  in
  let x =
    5 + Draw.(
      text ~x ~fg:conf.normal_foreground ~bg:conf.normal_background "%s|%s"
        state.compl.before_cursor state.compl.after_cursor
    )
  in
  match state.compl.after_matches with
  | [] -> x
  | t :: q ->
    let x' = draw_match ~hl: true conf x t in
    ignore (List.fold_left (draw_match conf) x'
              (fst (displayable_matches x' q)));
    x

let draw_window conf state =
  Draw.clear "#000000";
  let x = draw_matches conf state in
  Draw.mapdc ();
  x

exception Finished of string

let run_list { prompt ; compl } (conf : conf) =
  Draw.setup (not conf.bottom) conf.window_background conf.lines; 
  ignore (Draw.grabkeys ()); 
  let rec loop state =
    let last_x = draw_window conf { prompt ; compl = state } in
    let (key, str) = Draw.next_event () in
    match key with
    (* beurk *)
    | 0xff1b -> None
    | 0xff08 -> loop (remove state)
    | 0xff09 -> loop (complete state)
    | 0xff51 -> loop (left state)
    | 0xff53 -> loop (right state)
    | 0xff55 -> loop (pageup (displayable_matches last_x) state)
    | 0xff56 -> loop (pagedown (displayable_matches last_x) state)
    | 0xff0d ->
      let { after_matches ; before_cursor ; after_cursor ; _ } = state in
      let result =
        List.map (fun (_, s, _) -> s) state.entries @
          if before_cursor ^ after_cursor = "" then []
          else
            [try (fst (List.hd after_matches)).real
              with _ -> before_cursor ^ after_cursor]
      in
      Some result
    | _ ->
      loop (add_string str state)
  in
  loop compl

let run a b = Option.map (String.concat " ") (run_list a b)
