module Low_level = struct
  external setup : bool -> string -> int -> unit = "caml_setup"
  external width : unit -> int = "caml_width"
  external quit : unit -> unit = "caml_xquit"
  external grabkeys : unit -> bool = "caml_grabkeyboard"
  external next_event : unit -> (int * string) = "caml_next_event"
  external draw_text : string -> (int * int) -> (bool * int * int) list -> (string * string * string) ->  int = "caml_drawtext"
  external mapdc : unit -> unit = "caml_mapdc"
  external size : string -> int = "caml_size"
  external clear : string -> unit = "caml_clear"
  external clear_line : int -> string -> unit = "caml_clear_line"
  external resize : int -> unit = "caml_resize"
end

module Colors = struct 
  type t = {
    focus_foreground: string;
    focus_background: string;
    normal_foreground: string;
    normal_background: string;
    match_foreground: string;
    window_background: string;
  }
  let default = {
    normal_background = "#222222" ;
    normal_foreground = "#bbbbbb" ;
    focus_background = "#005577" ;
    focus_foreground = "#eeeeee" ;
    match_foreground = "#ff0000" ;
    window_background = "#000000" ;
  }
                 
end

type state = {
  colors: Colors.t;
  mutable x: int;
  mutable line: int;
}

let colors s = s.colors

let quit ~state:_ = Low_level.quit ()
let setup ~topbar ~colors ~lines = 
  Low_level.setup topbar colors.Colors.window_background lines;
  if Low_level.grabkeys () then
    Some { colors; x = 0; line = 0 }
  else
    None

let resize ~state:_ ~lines =
  Low_level.resize lines

let width ~state:_ = Low_level.width ()
let text_width ~state:_ s = Low_level.size s

module Draw = struct
    let set_line ~state ~line =
      state.line <- line
    let set_x ~state ~x = 
      state.x <- x
    let update_x ~state f =
      state.x <- f state.x

    let text_hl ~state ~result ~focus s = 
      let open Colors in
      let x' = Low_level.draw_text s (state.x, state.line) result
        ((if focus then state.colors.focus_foreground
          else          state.colors.normal_foreground),
         state.colors.match_foreground,
         if focus then state.colors.focus_background
         else          state.colors.normal_background)
      in state.x <- x'
          
    let text ~state ~focus fmt = 
      fmt |> Printf.kprintf (fun s ->
        text_hl ~result: [false, 0, String.length s] ~state  ~focus s)

    let clear_line line color = Low_level.clear_line line color

    let clear ~state = 
      state.x <- 0;
      state.line <- 0;
      Low_level.clear state.colors.Colors.window_background
    let map ~state:_ = Low_level.mapdc ()
end

module Key = struct
  type t =
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
    | Other of string

  let of_int (k, str) =
    match k with
    | 0xff1b -> Escape
    | 0xff51 -> Left
    | 0xff52 -> Up
    | 0xff53 -> Right
    | 0xff54 -> Down
    | 0xff0d -> Enter
    | 0xff09 -> Tab
    | 0xff08 -> Backspace
    | 0xff55 -> Scroll_up
    | 0xff56 -> Scroll_down
    | _ -> Other str
end

module Events = struct
  type t = 
    | Key of Key.t

  let poll ~state:_ ~timeout:_ =
    Some (Key (Key.of_int (Low_level.next_event ())))
end
