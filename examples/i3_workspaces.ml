let get_workspace prompt =
  let open Dmlenu in
  let app_state = {
    prompt ;
    compl = Completion.(make_state 
                          (Program ([Extra_sources.i3_workspaces ()], fun _ _ -> empty_program))) ;
  }
  in
  match run app_state default_conf with
  | None -> exit 0
  | Some ws -> ws

let () =
  try
    match Sys.argv.(1) with
    | "rename" ->
      let ws = get_workspace "rename to:" in
      Unix.execvp "i3-msg" [| "i3-msg"; "-q"; "rename"; "workspace"; "to"; ws |]
    | "move" ->
      let ws = get_workspace "move to:" in
      Unix.execvp "i3-msg"
        [| "i3-msg"; "-q"; "move"; "window"; "to"; "workspace"; ws |]
    | _ ->
      let ws = get_workspace "Go to:" in
      Unix.execvp "i3-msg" [| "i3-msg" ; "-q" ; "workspace" ; ws |]
  with e ->
    exit 1
