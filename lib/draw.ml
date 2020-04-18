module Color = struct
  type t = float * float * float

  let of_string (color: string) =
    let color_l =
      match String.to_seq color |> List.of_seq with
      | '#' :: xs -> xs
      | xs -> xs
    in
    let str_of_list l = String.of_seq (List.to_seq l) in
    let color_of_chars x0 x1 =
      Scanf.sscanf (str_of_list [x0; x1]) "%x"
        (fun i -> float i /. 255.)
    in
    try
      match color_l with
      | [r0; r1; g0; g1; b0; b1] ->
        Some (
          color_of_chars r0 r1,
          color_of_chars g0 g1,
          color_of_chars b0 b1
        )
      | _ ->
        None
    with Scanf.Scan_failure _ -> None

  let of_string_exn s =
    match of_string s with
    | Some c -> c
    | None ->
      failwith ("Color.of_string: invalid color " ^ s)
end

type state = {
  surf: Cairo.Surface.t;
  cairo: Cairo.context;
  pango: Pango.context;
  font: Pango.font;
  fontdesc: Pango.font_description;
}

let init ~font ~topbar =
  let open Backend in
  let () = X11.init ~topbar () in
  let surf = X11.get_cairo_surface () in
  let cairo = Cairo.create surf in
  Cairo.set_antialias cairo Cairo.ANTIALIAS_SUBPIXEL;
  let pango = Cairo_pango.create_context cairo in
  let fo = Cairo.Font_options.create () in
  Cairo.Font_options.set_hint_style fo Cairo.HINT_STYLE_FULL;
  Cairo.Font_options.set_antialias fo Cairo.ANTIALIAS_SUBPIXEL;
  Cairo.Font_options.set_subpixel_order fo Cairo.SUBPIXEL_ORDER_DEFAULT;
  Cairo.Font_options.set cairo fo;
  let fontdesc = Pango.Font.from_string font in
  let font = Pango.Context.load_font pango fontdesc in
  { cairo = cairo; pango; font; fontdesc; surf }

let terminate _state =
  Backend.X11.terminate ()

let render state ~height draw_f =
  Backend.X11.resize_height height;

  Cairo.Group.push state.cairo;

  draw_f ();

  Cairo.Group.pop_to_source state.cairo;
  Cairo.paint state.cairo;
  Cairo.Surface.flush state.surf;
  Backend.X11.flush ()

let cairo_ctx state = state.cairo

let set_source_rgb' cairo col =
  let r, g, b = col in
  Cairo.set_source_rgb cairo r g b

let set_source_rgb state col =
  set_source_rgb' state.cairo col

let get_pango_layout state text =
  let layout = Cairo_pango.create_layout state.cairo in
  Pango.Layout.set_text layout text;
  Pango.Layout.set_font_description layout state.fontdesc;
  Pango.Layout.set_single_paragraph_mode layout true;
  (* TODO: handle scaling *)
  layout

type prepared_text = {
  text: string;
  width: int; height: int;
  baseline: int;
  layout: Pango.layout;
}

let prepare_text state text =
  let layout = get_pango_layout state text in
  Cairo_pango.update_layout state.cairo layout;
  let width, height = Pango.Layout.get_pixel_size layout in
  let baseline = Pango.Layout.get_baseline layout / Pango.scale in
  { text; width; height; baseline; layout }

type aligned_texts = {
  aligned_height : int;
  aligned_baseline : int;
  aligned_texts : prepared_text list;
}

let prepare_aligned_texts state texts =
  let texts = List.map (prepare_text state) texts in
  let max_above, max_below =
    List.fold_left (fun (max_above, max_below) t ->
      max max_above t.baseline, max max_below (t.height - t.baseline))
      (0, 0) texts
  in
  { aligned_height = max_above + max_below;
    aligned_baseline = max_above;
    aligned_texts = texts }

let draw_text_hl_raw state
      ~color ~color_hl
      (text: prepared_text)
      (matching_result: (bool * int * int) list)
  =
  let matched, text_parts =
    List.fold_right (fun (m, start, stop) (matched, text_parts) ->
      (m :: matched,
       String.sub text.text start (stop - start) :: text_parts)
    ) matching_result ([], [])
  in
  let { aligned_texts; _ } =
    prepare_aligned_texts state text_parts in
  let x, y = Cairo.Path.get_current_point state.cairo in
  Cairo.save state.cairo;
  List.iter2 (fun m part ->
    if m then set_source_rgb state color_hl
    else set_source_rgb state color;
    let offset_y = float (text.baseline - part.baseline) in
    Cairo.rel_move_to state.cairo 0. offset_y;
    Cairo_pango.show_layout state.cairo part.layout;
    Cairo.rel_move_to state.cairo (float part.width) (-. offset_y);
  ) matched aligned_texts;
  Cairo.restore state.cairo;
  Cairo.move_to state.cairo x y

let draw_sharp_filled_rectangle cairo ~color ~x ~y ~w ~h =
  Cairo.save cairo;
  set_source_rgb' cairo color;
  Cairo.set_antialias cairo Cairo.ANTIALIAS_NONE;
  Cairo.rectangle cairo x y ~w ~h;
  Cairo.fill cairo;
  Cairo.restore cairo;
  ()

let draw_sharp_rectangle cairo ~color ~x ~y ~w ~h =
  Cairo.save cairo;
  set_source_rgb' cairo color;
  Cairo.set_antialias cairo Cairo.ANTIALIAS_NONE;
  (* not completely sure about this *)
  Cairo.rectangle cairo (x +. 0.5) (y +. 0.5) ~w:(w-.1.) ~h:(h-.1.);
  Cairo.stroke cairo;
  Cairo.restore cairo;
  ()

let draw_text_aux ?color_background ~color_foreground ?(xoff = 0)
      ~height ~baseline ~state text show_layout
  =
  let prepared = prepare_text state text in
  let x, y = Cairo.Path.get_current_point state.cairo in
  (match color_background with
   | None -> ()
   | Some color ->
     draw_sharp_filled_rectangle state.cairo ~color
       ~x ~y ~w:(float (prepared.width + 2 * xoff)) ~h:(float height));
  let yoff = float (baseline - prepared.baseline) in
  Cairo.move_to state.cairo (x +. float xoff) (y +. yoff);
  Cairo.save state.cairo;
  set_source_rgb state color_foreground;
  show_layout state prepared;
  Cairo.restore state.cairo;
  Cairo.rel_move_to state.cairo (float (prepared.width + xoff)) (-. yoff)

let draw_text ?color_background ~color_foreground ?xoff
      ~height ~baseline ~state text =
  draw_text_aux ?color_background ~color_foreground ?xoff
    ~height ~baseline ~state text (fun state prepared ->
      Cairo_pango.show_layout state.cairo prepared.layout)

let draw_text_hl ?color_background ~color_foreground ?xoff
      ~color_hl ~height ~baseline ~state text matching_result =
  draw_text_aux ?color_background ~color_foreground ?xoff
    ~height ~baseline ~state text (fun state prepared ->
      draw_text_hl_raw state ~color:color_foreground ~color_hl
        prepared matching_result)

let text_width state txt =
  let text_size = prepare_text state txt in
  text_size.width

(* No precise text positioning computations should rely on this. This is just to
   get a somewhat reasonable value for the width of a character space in the
   font, typically, to use as spacing in UI elements. *)
let approx_char_width state =
  let metrics =
    Pango.Context.get_metrics state.pango state.fontdesc None in
  Pango.Font.get_approximate_char_width metrics / Pango.scale
