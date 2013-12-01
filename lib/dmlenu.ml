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


let draw_match conf x (candidate, list) =
  10 + Draw.draw_text candidate.display x list
         (conf.normal_foreground, conf.match_foreground, conf.normal_background)
  
type app_state = {
  compl: state;
  prompt: string;
}

let draw_matches conf state =
  let rec go index = function
    | [] -> []
    | ({display} as candidate, rest) :: q -> 
      let size = Draw.size (display) + 10 in
      if (index + size) > Draw.width () then [] else
      (candidate, rest) :: go (index + size) q
  in
  let x =
    if state.prompt = "" then 0 else
    Draw.(text ~x:0 ~fg:conf.focus_foreground ~bg:conf.focus_background "%s" state.prompt)
  in
  let x =
    5 + Draw.(
      text ~x ~fg:conf.normal_foreground ~bg:conf.normal_background "%s|%s"
        state.compl.before_cursor state.compl.after_cursor
    )
  in
  ignore (List.fold_left (draw_match conf) x (go x state.compl.matches))

let draw_window conf state =
  Draw.clear "#000000";
  draw_matches conf state;
  Draw.mapdc ()  

exception Finished of string

let run { prompt ; compl } (conf : conf) =
  Draw.setup (not conf.bottom) conf.window_background conf.lines; 
  ignore (Draw.grabkeys ()); 
  let rec loop state =
    draw_window conf { prompt ; compl = state } ;
    let (key, str) = Draw.next_event () in
    match key with
    (* beurk *)
    | 0xff1b -> None
    | 0xff08 -> loop (remove state)
    | 0xff09 -> loop (complete state)
    | 0xff51 -> loop (left state)
    | 0xff53 -> loop (right state)
    | 0xff0d ->
      let { matches ; before_cursor ; after_cursor ; _ } = state in
      let result =
        try (fst (List.hd matches)).real
        with _ -> before_cursor ^ after_cursor
      in
      Some result
    | _ ->
      Printf.eprintf "keysym = %x\n%!" key ;
      loop (add_string str state)
  in
  loop compl
