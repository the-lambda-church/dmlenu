open Batteries

let list_episodes show_name =
  let ic = Unix.open_process_in ("series " ^ show_name) in
  let rec loop () =
    try 
      let episode = input_line ic in
      let (id, name) = String.split episode ~by:"\t" in
      (name, id) :: loop ()
    with End_of_file -> []
  in
  let episodes = loop () in
  ignore (Unix.close_process_in ic) ;
  Sources.from_list episodes

let list_shows =
  let ic = Unix.open_process_in "series" in
  let rec loop () =
    try 
      let show = input_line ic in
      let show_name, _ = String.split show ~by:" " in
      let episodes_src = lazy (list_episodes show_name) in
      Sources.add_subcommand ~name:("series" ^ show_name) episodes_src ; (* Ugly *)
      show_name :: loop ()
    with End_of_file -> []
  in
  let shows = loop () in
  ignore (Unix.close_process_in ic) ;
  Sources.from_list_ shows

let () =
  Sources.add_subcommand ~name:"series" (Lazy.from_val list_shows) ;
  Sources.add_subcommand ~name:"mpcload" Extra_sources.mpc_playlists ;
  Sources.add_subcommand ~name:"chromium" Extra_sources.chromium_bookmarks

let run =
  let open Dmlenu in
  let effect source_name =
    Matching.(set_match_query_fun @@ fuzzy_match ~case:false) ;
    if String.starts_with source_name "series" then Dmlenu.set_max_lines 10
  in
  let app_state = {
    prompt = "" ;
    compl = Completion.make_state (Sources.binaries_with_subcommands ~effect ()) ;
  }
  in
  match run_list app_state default_conf with
  | [] -> ()
  | prog :: params as lst -> Unix.execv prog (Array.of_list lst)
