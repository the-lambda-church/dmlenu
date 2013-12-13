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


let draw_match ?(hl = false) line conf x (candidate, list) =
  10 + Draw.draw_text candidate.display (x, line) list
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
      if (index + size) > Draw.width () - 5 then [], l else
      let a, b = go (index+size) q in
      (candidate, rest) :: a, b
  in
  go x

let init_draw conf state =
  let line = 0 in
  let x =
    if state.prompt = "" then 0 else
    Draw.(text ~line ~x:0 ~fg:conf.focus_foreground ~bg:conf.focus_background "%s" state.prompt)
  in
  let x = List.fold_left Draw.(fun x (_, _, display) ->
    2+text ~line ~x ~fg: conf.focus_foreground ~bg:conf.focus_background "%s" display) x state.compl.entries
  in
  5 + Draw.(
    text ~line ~x ~fg:conf.normal_foreground ~bg:conf.normal_background "%s|%s"
      state.compl.before_cursor state.compl.after_cursor
  )

let draw_matches line x conf state =
  match state.compl.after_matches with
  | [] -> x
  | t :: q ->
    let tuple =
      conf.normal_foreground, conf.match_foreground, conf.normal_background
    in
    let x =
      match state.compl.before_matches with
      | [] -> x
      | _ -> Draw.draw_text "<" (x, line) [(false, 0, 1)] tuple
    in
    let x' = draw_match ~hl: true line conf x t in
    let displayable, rest = displayable_matches x' q in
    let border = List.fold_left (draw_match line conf) x' displayable in
    begin match rest with
    | [] -> ()
    | _ -> ignore (Draw.draw_text ">" (border, line) [(false, 0, 1)] tuple)
    end ;
    x

let one_match_per_line conf state =
  let m = state.compl.after_matches in
  let offset, m =
    let len = List.length m in
    if len >= conf.lines then 0, m else
    let offset = conf.lines - len in
    offset, List.(rev @@ take offset state.compl.before_matches) @ m
  in
  if m = [] then () else
  let size = min (List.length m) conf.lines in
  let _ = Draw.resize size in
  let () = Draw.clear "#000000" in
  List.iteri (fun line s -> 
    let hl = line = offset in
    ignore (draw_match ~hl (line + 1) conf 5 s)
  ) m

let draw_window conf state =
  Draw.clear "#000000";
  let x = 
    if conf.lines = 0 then
      let x = init_draw conf state in
      draw_matches 0 x conf state
    else (
      one_match_per_line conf state ;
      init_draw conf state
    )
  in
  Draw.mapdc ();
  x

exception Finished of string

let run_list { prompt ; compl } (conf : conf) =
  Draw.setup (not conf.bottom) conf.window_background conf.lines; 
  ignore (Draw.grabkeys ()); 
  let rec loop state =
    let last_x = draw_window conf { prompt ; compl = state } in
    let (key, str) = Draw.next_event () in
    let ret k = 
      Draw.quit ();
      k
    in
    match key with
    (* beurk *)
    | 0xff1b -> ret []
    | 0xff08 -> loop (remove state)
    | 0xff09 -> loop (complete state)

    | 0xff51
    | 0xff52 -> loop (left state)
    | 0xff53
    | 0xff54 -> loop (right state)

      (* TODO: fix that [last_x] shit. *)
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
      ret result
    | _ ->
      loop (add_string str state)
  in
  loop compl

let run a b =
  match run_list a b with
  | []  -> None
  | lst -> Some (String.concat " " lst)
