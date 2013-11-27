(** More specific sources.
  
    These are environment dependent, and require yojson to compile. They are
    shipped with the [dmlenu.extra] findlib package. *)

val chromium_bookmarks : unit -> Sources.t
(** Offers completion on bookmarks names, and returns the bookmark url. *)

val mpc_playlists : unit -> Sources.t
(** List of mpc playlists. Completion can then be used with commands like
    [mpc load]. *)

val i3_workspaces : unit -> Sources.t
(** List of existing i3 workspaces. *)
