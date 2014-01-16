open Batteries
open Cmdliner

module Parameter = struct
    let normal_background_default = 
      try Sys.getenv "DMENU_NORMAL_BACKGROUND" with _ -> "#222222"
    let normal_foreground_default = 
      try Sys.getenv "DMENU_NORMAL_FOREGROUND" with _ -> "#bbbbbb"
    let focus_background_default = try Sys.getenv "DMENU_FOCUS_BACKGROUND" with _ -> "#005577"
    let focus_foreground_default = try Sys.getenv "DMENU_FOCUS_FOREGROUND" with _ -> "#eeeeee"
    let match_foreground_default = try Sys.getenv "DMENU_FOCUS_FOREGROUND_MATCH" with _ -> "#ff0000"

    let window_background = "#000000"

    let prompt =
      let doc = "Prompt to be displayed on the left of the input field" in
      Arg.(value & opt string "" & info ["p"; "prompt"] ~docv:"PROMPT" ~doc)

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
      Arg.(value & flag & info ["b"; "bottom"] ~doc)

    let stdin = 
      let doc = "If set, read the candidates off stdin" in
      Arg.(value & flag & info ["s"; "stdin"] ~doc)
end

let run prompt stdin topbar focus_foreground focus_background normal_foreground
      normal_background match_foreground window_background lines = 
  let () = Matching.(set_match_query_fun @@ subset ~case:false) in
  let colors = { X.Colors.focus_foreground; focus_background;
                 normal_foreground; normal_background; match_foreground; window_background } in
  let program = 
    if stdin then Program.singleton (Source.stdin ())
    else {
      Program.sources = [ Source.binaries ];
      completion = [];
      transition = fun o -> Extra_sources.stm_from_file o#display
    }
  in    
  match Dmlenu.run ~prompt ~lines ~topbar ~colors program with
  | None -> ()
  | Some s -> print_endline s

let info = 
  let doc = "print a menu with customizable completion" in
  Term.info "dmlenu" ~version:"0.0" ~doc ~man:[]

let dmlenu =
  Parameter.(
    Term.(
      pure run $ prompt $ stdin $  bottom $ focus_foreground $ focus_background $
        normal_foreground $ normal_background $ match_foreground $
        window_background $ lines
    )
  )

let _ =
  match Term.eval (dmlenu, info) with
  | `Error _ -> exit 1
  | _ -> exit 0
