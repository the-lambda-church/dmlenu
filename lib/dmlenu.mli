(** Main module of the library.

    Use it to build your own dmenu-like application. *)

type conf = {
  stdin: bool;
  bottom: bool;
  focus_foreground: string;
  focus_background: string;
  normal_foreground: string;
  normal_background: string;
  match_foreground: string;
  lines: int;
  window_background: string;
}

val default_conf : conf

type app_state = {
  compl: Completion.state;
  prompt: string;
}

(** {3 Execution} *)

val run : app_state -> conf -> unit
  (** [run initial_state conf] creates the window and handles user inputs.
      Outputing the selection on stdout at the end. *)
