let () =
  Sources.add_subcommand ~name:"mpcload" Extra_sources.mpc_playlists ;
  Sources.add_subcommand ~name:"chromium" Extra_sources.chromium_bookmarks

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
    compl = Completion.make_state (Sources.binaries_with_subcommands) ;
  }
  in
  match run_list ~source_transition_hook app_state default_conf with
  | [] -> ()
  | prog :: params as lst -> Unix.execv prog (Array.of_list lst)
