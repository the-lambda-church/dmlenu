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

let run (init_state : app_state) (conf : conf) =
  let state = ref init_state in
  Draw.setup (not conf.bottom) conf.window_background conf.lines; 
  ignore (Draw.grabkeys ()); 
  draw_window conf !state;
  let compl_fun key str =
    match key with
    (* beurk *)
    | 0xff08 -> remove
    | 0xff09 -> complete
    | 0xff51 -> left
    | 0xff53 -> right
    | 0xff0d ->
      begin fun { matches ; before_cursor ; after_cursor ; _ } ->
        let result =
          try (fst (List.hd matches)).real
          with _ -> before_cursor ^ after_cursor
        in
        print_endline result ;
        exit 0
      end
    | _ -> add_string str
  in
  Draw.run (fun (k, s) -> 
    state := { !state with compl = compl_fun k s !state.compl } ;
    draw_window conf !state
  )
