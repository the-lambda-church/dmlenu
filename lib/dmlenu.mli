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

val run : ?source_transition_hook:(Completion.state -> conf -> conf) -> app_state -> conf -> string option
(** [run initial_state conf] creates the window and handles user inputs.
    Returns [Some user_input] when the user valid a completion (i.e. hits RET)
    or [None] when the user cancels (i.e. hits ESC)

    N.B. this function is blocking.

    The [source_transition_hook] parameter is to be used when you want to
    perform side effects, or update the configuration when a transition happens.
    The reason why you would want use this hook insteal of directly performing
    your computation in the transition function of the state machine (see
    {!Completion}) is:
    - You cannot modify the configuration in the machine transition function
      (although we could allow it).
    - This function will be called when the user completes something, but also
      when he removes some of its input : i.e. when we backtrack in the
      state_machine/automata, whereas the transition function is called only
      when the user completes.
*)

(* TODO: move before [run], more explicit documentation *)
val run_list : ?source_transition_hook:(Completion.state -> conf -> conf) -> app_state -> conf -> string list
(** Same as {!run} but returns the list of completed tokens *)
