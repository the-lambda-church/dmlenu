(* :batteries,graphics: *)
open Batteries
open Completion
(* Variable to be made parametric *)
let prompt = ref "Prompt:"
let draw_match (candidate, list) =
  try
    List.iter (fun (b, start, stop) ->
      if b then Graphics.(set_color red)
      else Graphics.(set_color black);
      Graphics.draw_string 
        (String.sub candidate.display start (stop - start)))
    list;
  Graphics.draw_string "  "
  with _ -> ()
  
let total_size = 
  let inp = Unix.open_process_in ~autoclose: true
    ~cleanup: true "xdpyinfo | grep dimension"
  in
  Scanf.sscanf (IO.read_line inp) " %s %d" 
    (fun _ a -> ignore (Unix.close_process_in inp); a)

type app_state = {
  compl: completion_state;
  prompt: string;
}
let draw_matches state =
  let rec go index = function
    | [] -> []
    | ({display} as candidate, rest) :: q -> 
       let size = fst (Graphics.text_size (display ^ " ")) in
       if Graphics.(current_x () + size) > total_size then
        []
      else
        (candidate, rest) :: go (index + size) q
  in
  Graphics.open_graph (Printf.sprintf " %dx8" total_size);
  Graphics.set_window_title "dmlenu";
  Graphics.moveto 0 0;
  Graphics.(draw_string 
              (Printf.sprintf "%s%s|%s  " state.prompt 
                 state.compl.before_cursor state.compl.after_cursor));
  List.iter draw_match (go (Graphics.current_x()) state.compl.matches)

(*let candidates = IO.lines_of stdin |> List.of_enum*)

let init_state = {
  prompt = "Prompt:";
  compl = {
    before_cursor = ""; after_cursor = "";
    sources = [[], filename];
    matches = [];
  }
}
let rec main state =
  draw_matches state;
  match Graphics.read_key ()  with
    | (* escape *) '\027' -> Graphics.close_graph ()
    | (* enter *) '\r' -> 
      print_endline 
        (if state.compl.matches = [] then state.compl.before_cursor
         else (fst (List.hd state.compl.matches)).display)
    | (* backspace *) '\t' -> main { state with compl = complete state.compl }
    | (* backspace *) '\b' -> main { state with compl = remove state.compl }
    | (* backspace *) '<' -> main { state with compl = left state.compl }
    | (* any other *) c -> main { state with compl = add_char c state.compl }

let _ = main init_state
