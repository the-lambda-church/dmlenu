(** X operations *)

type state
(** The type of the X context *)

(** Colors we need to draw text *)
module Colors : sig
  type t = {
    focus_foreground: string;
    focus_background: string;
    normal_foreground: string;
    normal_background: string;
    match_foreground: string;
    window_background: string;
  }
  val default : t
  (** Default set of colors -- hardcoded at the moment *)
end
val setup : topbar: bool -> colors: Colors.t -> lines: int -> state option
(** Set up X.

    - [topbar] : true if the window should be at the top
    - [colors] : the colors to use to render text
    - [lines] : the size in lines of the window (at least one).

   It returns None if the setting up failed.
*)

val colors : state -> Colors.t

val quit : state: state -> unit
(* Destroy the window and release the keyboard *)
val resize : state: state -> lines: int -> unit
(** Resize the given X state *)

val width : state: state -> int
(** Returns the width of the window *)

val text_width : state: state -> string -> int
(** Returns the width of a text *)

module Key : sig
  type keysym =
    | Escape
    | Left
    | Right
    | Up
    | Down
    | Enter
    | Tab
    | Backspace
    | Scroll_up
    | Scroll_down
    | K_b
    | K_f
    | K_p
    | K_n
    | K_h
    | K_j
    | K_k
    | K_l
    | Other

  type modifier =
    | Shift
    | Lock
    | Control
    | Mod1
    | Mod2
    | Mod3
    | Mod4
    | Mod5
end

module Events : sig
  type t = 
  | Key of (Key.keysym * Key.modifier list) option * string

  val wait : unit -> t
  (** wait for the next X event *)
end

module Draw : sig
    (** Drawing text *)
    
    val set_line : state: state -> line: int -> unit
    (** Set the current line of drawing *)

    val set_x : state: state -> x: int -> unit
    (** sets the current X position to draw *)

    val update_x : state: state -> (int -> int) -> unit
    (** Updates the current X position *)

    val text: state: state -> focus: bool -> ('a, unit, string, unit) format4 -> 'a
    (** Draw some text at the current position. [focus] is used to
        know which colors to use *)

    val text_hl: state: state -> result: Matching.result -> focus: bool -> 
      string -> unit
    (** Draw a text by highlighting some parts of it according to [result] *)

    val clear : state: state -> unit
    (** Clear the screen with the background *)

    val clear_line : int -> string -> unit

    val map : state: state -> unit
    (** Map the window on the screen *)
end

(** Low-level interface *)
module Low_level : sig
  external setup : bool -> string -> int -> unit = "caml_setup"
  external quit : unit -> unit = "caml_xquit"
  external width : unit -> int = "caml_width"
  external grabkeys : unit -> bool = "caml_grabkeyboard"
  external next_event : unit -> (int * int * bool * string) = "caml_next_event"
  external draw_text : string -> (int * int) -> (bool * int * int) list -> (string * string * string) ->  int = "caml_drawtext"
  external mapdc : unit -> unit = "caml_mapdc"
  external size : string -> int = "caml_size"
  external clear : string -> unit = "caml_clear"
  external resize : int -> unit = "caml_resize"
end
