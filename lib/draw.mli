module Color : sig
  type t
  val of_string : string -> t option
  val of_string_exn : string -> t
end

(** {2} Low-level drawing primitives. Used internally by the [Ui] module. *)

type state

val init : font:string -> topbar:bool -> state
val render : state -> height:int -> (unit -> unit) -> unit
val terminate : state -> unit

val cairo_ctx : state -> Cairo.context
val set_source_rgb : state -> Color.t -> unit

type prepared_text = {
  text: string;
  width: int; height: int;
  baseline: int;
  layout: Pango.layout;
}

type aligned_texts = {
  aligned_height : int;
  aligned_baseline : int;
  aligned_texts : prepared_text list;
}

val text_width : state -> string -> int
val approx_char_width : state -> int
val prepare_text : state -> string -> prepared_text
val prepare_aligned_texts : state -> string list -> aligned_texts

val draw_text :
  ?color_background:Color.t -> color_foreground:Color.t ->
  ?xoff:int -> height:int -> baseline:int -> state:state ->
  string -> unit

val draw_text_hl :
  ?color_background:Color.t -> color_foreground:Color.t -> ?xoff:int ->
  color_hl:Color.t -> height:int -> baseline:int -> state:state ->
  string -> (bool * int * int) list -> unit

val draw_sharp_rectangle :
  Cairo.context -> color:Color.t ->
  x:float -> y:float -> w:float -> h:float ->
  unit

val draw_sharp_filled_rectangle :
  Cairo.context -> color:Color.t ->
  x:float -> y:float -> w:float -> h:float ->
  unit

