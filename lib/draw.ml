let horizontal_padding = 5.
let vertical_padding = 1.5

type state = {
  surf: Cairo.Surface.t;
  cairo: Cairo.context;
  font: Pango.font_description;
}

let parse_color (color: string) =
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

let validate_color (color: string): bool =
  match parse_color color with Some _ -> true | None -> false

let parse_color_exn color =
  match parse_color color with
  | Some c -> c
  | None ->
    raise (Invalid_argument ("parse_color_exn: " ^ color))

(*
let markup_of_matching_result
      ~(color: string)
      ~(text: string)
      (result: (bool * int * int) list): string
  =
  let b = Buffer.create 32 in
  List.iter (fun (is_hl, start, stop) ->
    let text_part = String.sub text start (stop - start) in
    if is_hl then (
      Buffer.add_string b "<span foreground='";
      Buffer.add_string b color;
      Buffer.add_string b "'>"
    );
    Buffer.add_string b text_part;
    if is_hl then (
      Buffer.add_string b "</span>"
    );
  ) result;
  Buffer.contents b
*)

let get_pango_layout ?(markup = false) state text =
  let layout = Cairo_pango.create_layout state.cairo in
  (if markup then
     (* TODO: error reporting in case of invalid markup?
        Currently pango prints a warning on stderr *)
     Pango.Layout.set_markup layout text
   else
     Pango.Layout.set_text layout text);
  Pango.Layout.set_font_description layout state.font;
  Pango.Layout.set_single_paragraph_mode layout true;
  (* TODO: handle scaling *)
  layout

type prepared_text = {
  text: string;
  width: int; height: int;
  baseline: int;
  layout: Pango.layout;
}

let prepare_text ?markup state text =
  let layout = get_pango_layout ?markup state text in
  Cairo_pango.update_layout state.cairo layout;
  let width, height = Pango.Layout.get_pixel_size layout in
  let baseline = Pango.Layout.get_baseline layout / Pango.scale in
  { text; width; height; baseline; layout }

type aligned_texts = {
  aligned_height : int;
  aligned_baseline : int;
  aligned_texts : prepared_text list;
}

let prepare_aligned_texts ?markup state texts =
  let texts = List.map (prepare_text ?markup state) texts in
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
  let r, g, b = color and rhl, ghl, bhl = color_hl in
  Printf.printf "text: \'%s\'\n" text.text;
  Printf.printf "matching res:";
  List.iter (fun (b, start, stop) ->
    Printf.printf " %b, %d, %d |" b start stop
  ) matching_result;
  print_endline "";

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
    if m then Cairo.set_source_rgb state.cairo rhl ghl bhl
    else Cairo.set_source_rgb state.cairo r g b;
    let offset_y = float (text.baseline - part.baseline) in
    Cairo.rel_move_to state.cairo 0. offset_y;
    Cairo_pango.show_layout state.cairo part.layout;
    Cairo.rel_move_to state.cairo (float part.width) (-. offset_y);
  ) matched aligned_texts;
  Cairo.restore state.cairo;
  Cairo.move_to state.cairo x y

let draw_sharp_filled_rectangle cairo ~color ~x ~y ~w ~h =
  Cairo.save cairo;
  let r, g, b = color in
  Cairo.set_source_rgb cairo r g b;
  Cairo.set_antialias cairo Cairo.ANTIALIAS_NONE;
  Cairo.rectangle cairo x y ~w ~h;
  Cairo.fill cairo;
  Cairo.restore cairo;
  ()

let draw_sharp_rectangle cairo ~color ~x ~y ~w ~h =
  Cairo.save cairo;
  let r, g, b = color in
  Cairo.set_source_rgb cairo r g b;
  Cairo.set_antialias cairo Cairo.ANTIALIAS_NONE;
  (* not completely sure about this *)
  Cairo.rectangle cairo (x +. 0.5) (y +. 0.5) ~w:(w-.1.) ~h:(h-.1.);
  Cairo.stroke cairo;
  Cairo.restore cairo;
  ()

let draw_text_aux ?markup ?color_background ~color_foreground ?(xoff = 0)
      ~height ~baseline ~state text show_layout
  =
  let prepared = prepare_text ?markup state text in
  let x, y = Cairo.Path.get_current_point state.cairo in
  (match color_background with
   | None -> ()
   | Some color ->
     draw_sharp_filled_rectangle state.cairo ~color
       ~x ~y ~w:(float (prepared.width + 2 * xoff)) ~h:(float height));
  let yoff = float (baseline - prepared.baseline) in
  Cairo.move_to state.cairo (x +. float xoff) (y +. yoff);
  Cairo.save state.cairo;
  let r, g, b = color_foreground in
  Cairo.set_source_rgb state.cairo r g b;
  show_layout state prepared;
  Cairo.restore state.cairo;
  Cairo.rel_move_to state.cairo (float (prepared.width + xoff)) (-. yoff)

let draw_text ?markup ?color_background ~color_foreground ?xoff
      ~height ~baseline ~state text =
  draw_text_aux ?markup ?color_background ~color_foreground ?xoff
    ~height ~baseline ~state text (fun state prepared ->
      Cairo_pango.show_layout state.cairo prepared.layout)

let draw_text_hl ?markup ?color_background ~color_foreground ?xoff
      ~color_hl ~height ~baseline ~state text matching_result =
  draw_text_aux ?markup ?color_background ~color_foreground ?xoff
    ~height ~baseline ~state text (fun state prepared ->
      draw_text_hl_raw state ~color:color_foreground ~color_hl
        prepared matching_result)

let init ~font ~topbar =
  let open Backend in
  let () = X11.init ~topbar () in
  let surf = X11.get_cairo_surface () in
  let cairo = Cairo.create surf in
  Cairo.set_antialias cairo Cairo.ANTIALIAS_SUBPIXEL;
  let fo = Cairo.Font_options.create () in
  Cairo.Font_options.set_hint_style fo Cairo.HINT_STYLE_FULL;
  Cairo.Font_options.set_antialias fo Cairo.ANTIALIAS_SUBPIXEL;
  Cairo.Font_options.set_subpixel_order fo Cairo.SUBPIXEL_ORDER_DEFAULT;
  Cairo.Font_options.set cairo fo;
  let font = Pango.Font.from_string font in
  { cairo = cairo; font; surf }

let terminate _state =
  Backend.X11.terminate ()

let render state height draw_f =
  Backend.X11.resize_height height;

  Cairo.Group.push state.cairo;

  (* useless? *)
  Cairo.save state.cairo;
  Cairo.set_operator state.cairo Cairo.CLEAR;
  Cairo.paint state.cairo;
  Cairo.restore state.cairo;

  draw_f ();

  Cairo.Group.pop_to_source state.cairo;
  Cairo.paint state.cairo;
  Cairo.Surface.flush state.surf;
  Backend.X11.flush ()
