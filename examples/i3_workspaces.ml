let get_workspace prompt =
  Matching.(set_match_query_fun @@ fuzzy_match ~case:false) ;
  let compl =
    let open Completion in
    make_state { dummy_machine with ex_sources = [Extra_sources.i3_workspaces] }
  in
  match Dmlenu.(run { prompt ; compl } default_conf) with
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
