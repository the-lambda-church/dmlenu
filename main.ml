(* :batteries,graphics: *)
open Batteries
open Completion
open Cmdliner
module Parameter = struct
    let prompt = 
      let doc = "Prompt of the menu" in
      Arg.(value & opt string "" & info ["p"; "prompt"] ~docv: "PROMPT" ~doc)
    let normal_background_default = 
      try Sys.getenv "DMENU_NORMAL_BACKGROUND" with _ -> "#222222"
    let normal_foreground_default = 
      try Sys.getenv "DMENU_NORMAL_FOREGROUND" with _ -> "#bbbbbb"
    let focus_background_default = try Sys.getenv "DMENU_FOCUS_BACKGROUND" with _ -> "#005577"
    let focus_foreground_default = try Sys.getenv "DMENU_FOCUS_FOREGROUND" with _ -> "#eeeeee"
    let match_foreground_default = try Sys.getenv "DMENU_FOCUS_FOREGROUND_MATCH" with _ -> "#ff0000"

    let window_background = "#000000"

    let normal_background = 
      let doc = "Normal background (for non focused elements)" in
      Arg.(value & opt string normal_background_default & info ["nb"] ~docv: "NB" ~doc)
    let focus_background = 
      let doc = "Focus background color (for focused elements)" in
      Arg.(value & opt string focus_background_default & info ["fb"] ~docv: "fB" ~doc)

    let normal_foreground = 
      let doc = "Normal foreground color (for non focused elements)" in
      Arg.(value & opt string normal_foreground_default & info ["nf"] ~docv: "NF" ~doc)

    let focus_foreground = 
      let doc = "Focus foreground color (for focused elements)" in
      Arg.(value & opt string focus_foreground_default & info ["ff"] ~docv: "ff" ~doc)

    let match_foreground = 
      let doc = "Color to display matches inside candidates" in
      Arg.(value & opt string match_foreground_default & info ["ff"] ~docv: "mf" ~doc)

    let window_background = 
      let doc = "Color of the window background" in
      Arg.(value & opt string window_background & info ["wb"] ~docv: "wb" ~doc)

    let lines = 
      let doc = "If set, display the candidates in lines" in
      Arg.(value & opt int 0 & info ["l"; "lines"] ~docv: "LINES" ~doc)

    let bottom = 
      let doc = "If set, display the menu at the bottom of the screen" in
      Arg.(value & opt bool false & info ["b"; "bottom"] ~docv: "BOTTOM" ~doc)

    let stdin = 
      let doc = "If set, read the candidates off stdin" in
      Arg.(value & opt bool false & info ["s"; "stdin"] ~docv: "STDIN" ~doc)
end

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
  10+Draw.draw_text candidate.display x list (conf.normal_foreground, conf.match_foreground, conf.normal_background)
  
type app_state = {
  compl: state;
  prompt: string;
}
let draw_matches conf state =
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
  let x = Draw.(text ~x ~fg:conf.focus_foreground ~bg:conf.focus_background "%s" state.prompt) in
  let x = 5+Draw.(text ~x ~fg:conf.normal_foreground ~bg:conf.normal_background
                    "%s|%s" state.compl.before_cursor state.compl.after_cursor) in
    ignore (List.fold_left (draw_match conf) x (go x state.compl.matches))

let draw_window conf state =
  Draw.clear "#000000";
  draw_matches conf state;
  Draw.mapdc ()  
(*let candidates = IO.lines_of stdin |> List.of_enum*)


let run stdin bottom focus_foreground focus_background 
    normal_foreground normal_background match_foreground window_background lines = 
  let conf = { lines; stdin; bottom; focus_foreground; focus_background; normal_foreground;
               normal_background; match_foreground; window_background } in
  let init_state = {
    prompt = "Prompt:";
    compl = make_state 
      (if conf.stdin then [Sources.stdin ()]
       else [Sources.( 
         concat " " binaries
           (kleene " " filename))])
  } in
  let state = ref init_state in
  Draw.setup (not conf.bottom) conf.window_background conf.lines; 
  ignore (Draw.grabkeys ()); 
  draw_window conf !state;
  Draw.run (fun (k, s) -> 
    state := { !state with compl = (match k with (* fuck you *)
    | 0xff08 -> remove
    | 0xff09 -> complete
    | 0xff51 -> left
    | 0xff53 -> right
    | 0xff0d -> fun {matches; before_cursor; after_cursor} ->
      print_endline (try (fst (List.hd matches)).real;
      with _ -> before_cursor ^ after_cursor);
      exit 0
    | _ -> add_string s) !state.compl };
    draw_window conf !state)
  
let info = 
  let doc = "print a menu with customizable completion" in
  let man  = [] in
  Term.info "dmlenu" ~version:"0.0" ~doc ~man

let dmlenu = Parameter.(Term.(pure run $ stdin $  bottom $ focus_foreground $ focus_background $
                                normal_foreground $ normal_background $ match_foreground $ window_background $ lines))
let _ = match Term.eval (dmlenu, info) with
  | `Error _ -> exit 1
  | _ -> exit 0
