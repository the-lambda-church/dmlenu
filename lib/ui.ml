open Base

(* The set of colors of the bar *)
module Colors = struct
  type t = {
    focus_foreground: Draw.Color.t;
    focus_background: Draw.Color.t;
    normal_foreground: Draw.Color.t;
    normal_background: Draw.Color.t;
    match_foreground: Draw.Color.t;
    window_background: Draw.Color.t;
  }

  let default = Draw.({
    normal_background = Color.of_string_exn "#222222";
    normal_foreground = Color.of_string_exn "#bbbbbb";
    focus_background = Color.of_string_exn "#005577";
    focus_foreground = Color.of_string_exn "#eeeeee";
    match_foreground = Color.of_string_exn "#ff0000";
    window_background = Color.of_string_exn "#000000";
  })
end

type line_geometry = {
  height : int;
  baseline : int;
}

type state = {
  dstate : Draw.state;
  xoff : int;
  (* Remember the previous height of the bar, to avoid repeatedly shrinking and
     increasing the height of the bar depending on the text; instead, we
     remember the previous height and only increase it when required (but never
     decrease it). *)
  mutable bar_geometry : line_geometry;
}

let cairo_ctx state = Draw.cairo_ctx state.dstate
let dstate state = state.dstate

let make dstate =
  let xoff =
    let Draw.{ height; _ } = Draw.prepare_text dstate "" in
    height / 2
  in
  { dstate; xoff; bar_geometry = { height = 0; baseline = 0 } }

let text_foreground ~focus colors =
  if focus then colors.Colors.focus_foreground
  else colors.Colors.normal_foreground

let text_background ~focus colors =
  if focus then colors.Colors.focus_background
  else colors.Colors.normal_background

let update_bar_geometry (s: state) (geom: line_geometry) =
  let voff = 2 in
  let geom =
    { height = geom.height + 2 * voff;
      baseline = geom.baseline + voff } in
  if s.bar_geometry.height >= geom.height then ()
  else s.bar_geometry <- geom

let current_bar_geometry (s: state): line_geometry =
  s.bar_geometry

let draw_text ?geometry ~focus ~colors state txt =
  let geometry = Option.value geometry ~default:state.bar_geometry in
  Draw.draw_text ~xoff:state.xoff
    ~color_background:(text_background ~focus colors)
    ~color_foreground:(text_foreground ~focus colors)
    ~height:geometry.height
    ~baseline:geometry.baseline
    ~state:state.dstate txt

let draw_text_hl ?geometry ~focus ~colors state txt matching_result =
  let geometry = Option.value geometry ~default:state.bar_geometry in
  Draw.draw_text_hl ~xoff:state.xoff
    ~color_background:(text_background ~focus colors)
    ~color_foreground:(text_foreground ~focus colors)
    ~color_hl:colors.match_foreground
    ~height:geometry.height
    ~baseline:geometry.baseline
    ~state:state.dstate txt matching_result

let clear_line ?geometry ~focus ~colors state =
  let geometry = Option.value geometry ~default:state.bar_geometry in
  let cairo = cairo_ctx state in
  let _, y = Cairo.Path.get_current_point cairo in
  if focus then
    Draw.draw_sharp_filled_rectangle cairo
      ~color:colors.Colors.focus_background ~x:0. ~y
      ~w:(Float.of_int (Backend.X11.get_width ()))
      ~h:(Float.of_int geometry.height);
  Cairo.move_to cairo 0. y;
  ()

let set_x state x =
  let cairo = cairo_ctx state in
  let _, y = Cairo.Path.get_current_point cairo in
  Cairo.move_to cairo x y

let text_width state txt =
  Draw.text_width state.dstate txt
