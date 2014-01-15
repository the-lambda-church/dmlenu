(** Main entry point of the library *)

type app_state = {
  colors: X.Colors.t; (** The set of colors to use *)
  lines: int; (** The maximum number of lines to use. (0 means no lines) *)
  prompt: string; (** The prompt to the user *)
  topbar: bool;  (** Shall dmlenu sit on the bottom or on the top of the screen? *)
  hook: (app_state -> app_state); (** Hook called whenever a token is added *)
  state: State.t; (** The state of the current engine *)
  xstate: X.state;
}
val run_list : 
  ?topbar: bool
  -> ?separator: string
  -> ?colors: X.Colors.t
  -> ?lines: int 
  -> ?prompt: string
  -> ?hook : (app_state -> app_state)
  -> Program.t
  -> string list
