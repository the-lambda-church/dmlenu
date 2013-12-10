external setup : bool -> string -> int -> unit = "caml_setup"
external width : unit -> int = "caml_width"
external quit : unit -> unit = "caml_xquit"
external grabkeys : unit -> bool = "caml_grabkeyboard"
external next_event : unit -> (int * string) = "caml_next_event"
external draw_text : string -> (int * int) -> (bool * int * int) list -> (string * string * string) ->  int = "caml_drawtext"
external mapdc : unit -> unit = "caml_mapdc"
external size : string -> int = "caml_size"
external clear : string -> unit = "caml_clear"
external resize : int -> unit = "caml_resize"

let text ~line ~x ~fg ~bg fmt = 
  Printf.kprintf (fun s -> draw_text s (x, line) [false, 0, String.length s] (fg, fg, bg)) fmt

