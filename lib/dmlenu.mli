(** Main module of the library.

    Use it to build your own dmenu-like application. *)

(** {2 Configuration} *)

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

val run : app_state -> conf -> string option
(** [run initial_state conf] creates the window and handles user inputs.
    Returns [Some user_input] when the user valid a completion (i.e. hits RET)
    or [None] when the user cancels (i.e. hits ESC)

    N.B. this function is blocking. *)

val run_list : app_state -> conf -> string list option
(** Same as {!run} but returns the list of completed tokens *)
