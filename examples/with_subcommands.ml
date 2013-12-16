let stm = 
  let open Completion in {
    ex_sources = [ Lazy.from_val Sources.binaries ] ;
    transition =
      fun ~display:cmd ~real:_ ->
        if cmd = "chromium" then
          iterate [ Extra_sources.chromium_bookmarks ]
        else {
          ex_sources = [ Lazy.from_val (Extra_sources.from_file cmd) ] ;
          transition = fun ~display ~real:_ ->
            if cmd = "mpc" && display = "load" then
              iterate [ Extra_sources.Mpc.playlists ]
            else
              Extra_sources.stm_from_file (cmd ^ display)
        }
  }

let run =
  let open Dmlenu in
  let source_transition_hook state conf =
    let source_name =
      List.map (fun (_, _, display) -> display) state.Completion.entries |>
      String.concat ""
    in
    let () = Printf.eprintf "%s\n%!" source_name in
    if source_name = "chromium" then (
      Matching.(set_match_query_fun @@ fuzzy_match ~case:false) ;
      { conf with lines = 20 }
    ) else (
      Matching.(set_match_query_fun @@ match_prefix ~case:false) ;
      { conf with lines = 0 }
    )
  in
  let app_state = {
    prompt = "" ;
    compl = Completion.make_state stm ;
  }
  in
  match run_list ~source_transition_hook app_state default_conf with
  | [] -> ()
  | prog :: params as lst -> Unix.execv prog (Array.of_list lst)
