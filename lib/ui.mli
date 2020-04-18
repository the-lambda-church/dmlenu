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

type state
val make : Draw.state -> state

val cairo_ctx : state -> Cairo.context
val dstate : state -> Draw.state

type line_geometry = {
  height : int;
  baseline : int;
}
val update_bar_geometry : state -> line_geometry -> unit
val current_bar_geometry : state -> line_geometry

val draw_text :
  ?geometry:line_geometry ->
  focus:bool -> colors:Colors.t -> state -> string -> unit

val draw_text_hl :
  ?geometry:line_geometry ->
  focus:bool -> colors:Colors.t -> state -> string ->
  (bool * int * int) list -> unit

val clear_line :
  ?geometry:line_geometry ->
  focus:bool -> colors:Colors.t -> state -> unit

val set_x : state -> float -> unit
val text_width : state -> string -> int
