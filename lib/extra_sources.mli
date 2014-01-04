(** More specific sources.
  
    These are environment dependent, and require yojson to compile. They are
    shipped with the [dmlenu.extra] findlib package. *)

val chromium_bookmarks : Sources.t Lazy.t
(** Offers completion on bookmarks names, and returns the bookmark url. *)

module Mpc : sig
  val current_playlist : Sources.t Lazy.t
  (** List songs currently in the playlist. *)

  val playlists : Sources.t Lazy.t
  (** List of mpc playlists. Completion can then be used with commands like
      [mpc load]. *)
end

val i3_workspaces : Sources.t Lazy.t
(** List of existing i3 workspaces. *)

val from_file : string -> Sources.t
(** [from_file filename] creates a source from the lines in
    [$HOME/.config/dmlenu/filename] (each line in this file is a candidate) *)

val stm_from_file : string -> Completion.state_machine
(** [stm_from_file f] starts by offering candidates returned by [from_file f],
    once a candidate [x] has been selected, it will behave as
    [stm_from_file (f ^ x)].  *)
