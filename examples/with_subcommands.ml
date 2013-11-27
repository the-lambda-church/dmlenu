module J = Yojson.Basic

let (/) = Filename.concat

(** {3 Extra Sources} *)

let chromium_bookmarks =
  let mk_entry ?prefix name url =
    let prefix str = match prefix with None -> str | Some s -> s / str in
    let name = prefix (if name = "" then url else name) in
    (name, url)
  in
  let file = Sys.getenv "HOME" / ".config/chromium/Default/Bookmarks" in
  let data = J.from_file file |> J.Util.member "roots" in
  let main =
    J.Util.(member "bookmark_bar" data |> member "children") |>
    J.Util.convert_each (fun entry ->
      let name = J.Util.(member "name" entry |> to_string) in
      let url  = J.Util.(member "url"  entry |> to_string) in
      mk_entry name url
    )
  in
  Sources.from_list main

let mpc_playlists =
  let ic = Unix.open_process_in "mpc lsplaylists" in
  let rec loop () =
    try 
      let playlist = input_line ic in
      playlist :: loop ()
    with End_of_file -> []
  in
  let playlists = loop () in
  ignore (Unix.close_process_in ic) ;
  Sources.from_list_ playlists

(** {3 Main} *)

let () =
  Sources.add_subcommand ~name:"chromium" chromium_bookmarks ;
  Sources.add_subcommand ~name:"mpcload" mpc_playlists

let run =
  let open Dmlenu in
  let app_state = {
    prompt = "" ;
    compl = Completion.make_state [ Sources.binaries_with_subcommands ] ;
  }
  in
  run app_state default_conf
