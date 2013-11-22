(* :batteries,graphics: *)
open Batteries
open Completion
(* Variable to be made parametric *)
let prompt = ref "Prompt:"
let bg = try Sys.getenv "DMENU_NORMAL_BACKGROUND" with _ -> "#ffffff"
let fg = try Sys.getenv "DMENU_NORMAL_FOREGROUND" with _ -> "#000000"
let bg_focus = try Sys.getenv "DMENU_FOCUS_BACKGROUND" with _ -> "#ff0000"
let fg_focus = try Sys.getenv "DMENU_FOCUS_FOREGROUND" with _ -> "#000000"
let fg_match = try Sys.getenv "DMENU_FOCUS_FOREGROUND_MATCH" with _ -> "#ff0000"
let draw_match x (candidate, list) =
  10+Draw.draw_text candidate.display x list (fg, fg_match, bg)
  
let total_size = 
  let inp = Unix.open_process_in ~autoclose: true
    ~cleanup: true "xdpyinfo | grep dimension"
  in
  Scanf.sscanf (IO.read_line inp) " %s %d" 
    (fun _ a -> ignore (Unix.close_process_in inp); a)

type app_state = {
  compl: state;
  prompt: string;
}
let draw_matches state =
  let rec go index = function
    | [] -> []
    | ({display} as candidate, rest) :: q -> 
       let size = Draw.size (display) + 10 in
       if (index + size) > Draw.width () then
        []
      else
        (candidate, rest) :: go (index + size) q
  in
  let x = 0 in
  let x = Draw.(text ~x ~fg:fg_focus ~bg:bg_focus "%s" state.prompt) in
  let x = 5+Draw.(text ~x ~fg ~bg "%s|%s" state.compl.before_cursor state.compl.after_cursor) in
    ignore (List.fold_left draw_match x (go x state.compl.matches))

let draw_window state =
  Draw.clear "#000000";
  draw_matches state;
  Draw.mapdc ()  
(*let candidates = IO.lines_of stdin |> List.of_enum*)

let init_state = {
  prompt = "Prompt:";
  compl = make_state Sources.(
    [concat " " 
        binaries (kleene "," filename)])
}

let state = ref init_state
let _ = Draw.setup true "#000000" 0; ignore (Draw.grabkeys ()); draw_window !state;
  Draw.run (fun (k, s) -> 
    state := { !state with compl = (match k with (* fuck you *)
    | 0xff08 -> remove
    | 0xff09 -> complete
    | 0xff51 -> left
    | 0xff53 -> right
    | _ -> add_string s) !state.compl };
    draw_window !state)
  
