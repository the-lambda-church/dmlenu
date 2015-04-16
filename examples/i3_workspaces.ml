let get_workspace prompt =
  Matching.(set_match_query_fun @@ fuzzy_match ~case:false) ;
  let compl = Engine.singleton Extra_sources.i3_workspaces in
  match Dmlenu.(run ~prompt compl) with
  | None -> exit 0
  | Some ws -> ws

let () =
  try
    match Array.to_list Sys.argv |> List.tl with
    | "rename" :: _ ->
      let ws = get_workspace "Rename to:" in
      Unix.execvp "i3-msg" [| "i3-msg"; "-q"; "rename"; "workspace"; "to"; ws |]
    | "move" :: _ ->
      let ws = get_workspace "Move to:" in
      Unix.execvp "i3-msg"
        [| "i3-msg"; "-q"; "move"; "window"; "to"; "workspace"; ws |]
    | _ ->
      let ws = get_workspace "Go to:" in
      Unix.execvp "i3-msg" [| "i3-msg" ; "-q" ; "workspace" ; ws |]
  with e ->
    exit 1
