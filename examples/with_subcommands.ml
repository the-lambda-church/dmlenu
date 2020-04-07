open Dmlenu
open Candidate

let stm =
  let open Engine in
  {
    sources = [ Source.binaries ] ;
    transition =
      fun cmd ->
        if cmd.display = "chromium" then
          iterate [ Dmlenu_extra.Sources.chromium_bookmarks ]
        else {
          sources = [ Dmlenu_extra.Sources.from_file cmd.display ] ;
          transition = fun arg ->
            if cmd.display = "mpc" && arg.display = "load" then
              iterate [ Dmlenu_extra.Sources.Mpc.playlists ]
            else
              Dmlenu_extra.Sources.stm_from_file (cmd.display ^ arg.display)
        }
  }

let run =
  let open App in
  let hook state =
    let source_name =
      List.map (fun (_, c) -> c.display) state.state.State.entries |>
      String.concat ""
    in
    let () = Printf.eprintf "%s\n%!" source_name in
    let several = State.MultiLine 20 in
    if source_name = "chromium" then (
      Matching.(set_match_query_fun @@ fuzzy_match ~case:false) ;
      { state with state = { state.state with State.layout = several } }
    ) else (
      Matching.(set_match_query_fun @@ match_prefix ~case:false) ;
      { state with state = { state.state with State.layout = several } }
    )
  in
  match run_list ~hook stm with
  | [] -> ()
  | prog :: _ as lst -> Unix.execv prog (Array.of_list lst)
