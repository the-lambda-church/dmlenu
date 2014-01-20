open Candidate

let stm = 
  let open Program in
  {
    sources = [ Source.binaries ] ;
    completion = [];
    transition =
      fun cmd ->
        if cmd.display = "chromium" then
          iterate [ Extra_sources.chromium_bookmarks ]
        else {
          sources = [ Extra_sources.from_file cmd.display ] ;
          completion = [];
          transition = fun arg ->
            if cmd.display = "mpc" && arg.display = "load" then
              iterate [ Extra_sources.Mpc.playlists ]
            else
              Extra_sources.stm_from_file (cmd.display ^ arg.display)
        }
  }

let run =
  let open Dmlenu in
  let hook state =
    let source_name =
      List.map (fun (_, c) -> c.display) state.state.State.entries |>
      String.concat ""
    in
    let () = Printf.eprintf "%s\n%!" source_name in
    if source_name = "chromium" then (
      Matching.(set_match_query_fun @@ fuzzy_match ~case:false) ;
      { state with lines = 20 }
    ) else (
      Matching.(set_match_query_fun @@ match_prefix ~case:false) ;
      { state with lines = 0 }
    )
  in
  match run_list ~hook stm with
  | [] -> ()
  | prog :: params as lst -> Unix.execv prog (Array.of_list lst)
