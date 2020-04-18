(** X operations *)

val init : ?monitor:int -> topbar:bool -> unit -> unit
(** Set up X.
    - [topbar] : true if the window should be at the top
    - [monitor] : id of the monitor where to display the bar.
      (starting from 1). An id of 0 selects the current monitor.
      Default value: 0.
*)

val terminate : unit -> unit
(** Destroy the window and release the keyboard. *)

val resize_height : int -> unit
(** Resize the window to the given height. *)

val flush : unit -> unit
(** Flush pending operations *)

val get_cairo_surface : unit -> Cairo.Surface.t
(** Get the cairo surface associated with the window. *)

val get_width : unit -> int
(** Returns the width of the window *)

val get_height : unit -> int
(** Returns the current height of the window *)

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
    | Key of Key.keysym * Key.modifier list * Uchar.t

  val wait : unit -> t
  (** Wait for the next X event *)
end
