external setup : bool -> string -> int -> unit = "caml_setup"
external width : unit -> int = "caml_width"
external grabkeys : unit -> bool = "caml_grabkeyboard"
external run : (int * string -> unit) -> unit = "caml_run"
external draw_text : string -> int -> (bool * int * int) list -> (string * string * string) ->  int = "caml_drawtext"
external mapdc : unit -> unit = "caml_mapdc"
external size : string -> int = "caml_size"
external clear : string -> unit = "caml_clear"

let text ~x ~fg ~bg fmt = 
  Printf.kprintf (fun s -> draw_text s x [false, 0, String.length s] (fg, fg, bg)) fmt

