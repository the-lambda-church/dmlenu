(** Main entry point of the library *)

module Colors : sig
  type t = {
    focus_foreground: Draw.Color.t;
    focus_background: Draw.Color.t;
    normal_foreground: Draw.Color.t;
    normal_background: Draw.Color.t;
    match_foreground: Draw.Color.t;
    window_background: Draw.Color.t;
  }
  val default : t
end
(** Colors for the UI elements of the bar *)

type app_state = {
  colors: Colors.t; (** The set of colors to use *)
  font: string;
  prompt: string; (** The prompt to the user *)
  topbar: bool;  (** Shall dmlenu sit on the bottom or on the top of the screen? *)
  hook: (app_state -> app_state); (** Hook called whenever a token is added *)
  state: State.t; (** The state of the current engine *)
  dstate: Draw.state; (** Data related to X *)
}
(** The current state of the application *)

val run_list : 
  ?topbar: bool
  -> ?separator: string
  -> ?colors: Colors.t
  -> ?font: string
  -> ?layout:State.layout
  -> ?prompt: string
  -> ?hook : (app_state -> app_state)
  -> Engine.t
  -> string list
(** Run a program and outputs the read tokens. You can override the
    default values for the parameter using optional parameters. The
    default values are
    - topbar: [true]
    - separator: [" "]
    - colors: [X.Colors.default]
    - lines: [0]
    - prompt: [""]
    - hook: the identity function
*)

val run : 
  ?topbar: bool
  -> ?separator: string
  -> ?colors: Colors.t
  -> ?layout:State.layout
  -> ?prompt: string
  -> ?hook : (app_state -> app_state)
  -> Engine.t
  -> string option
(* Same as {!run_list} but concatenates the tokens using the separator.
   If no token were read, returns [None] *)
