open Base
open Candidate
open Backend

type app_state = {
  colors: Ui.Colors.t; (** The set of colors to use *)
  font: string;
  prompt: string; (** The prompt to the user *)
  topbar: bool;  (** Shall dmlenu sit on the bottom or on the top of the screen? *)
  hook: (app_state -> app_state); (** Hook called whenever a token is added *)
  state: State.t; (** The state of the current engine *)
  ui_state: Ui.state; (** Data related to the drawing backend *)
}

let float = Float.of_int

let splith ui_state list =
  let rec go index = function
    | [] -> [], []
    | (candidate, rest) :: q as l->
      let size = Ui.text_width ui_state candidate.display + 10 in
      if (index + size) > X11.get_width () - 5 then [], l else
      let a, b = go (index+size) q in
      (candidate, rest) :: a, b
  in
  go 0 list

let incr ui_state = Cairo.rel_move_to (Ui.cairo_ctx ui_state) 5. 0.

let draw_horizontal { ui_state; state; colors; _ } =
  let open State in
  let candidates = state.candidates in
  if not (List.is_empty candidates.Pagination.unvisible_left) then (
    Ui.draw_text ~colors ~focus:false ui_state "<";
    incr ui_state
  );
  Sequence.iter ~f:(fun ((candidate, result), visible) ->
    Ui.draw_text_hl ~colors ~focus:visible ui_state candidate.display result;
    incr ui_state
  ) (Pagination.visible candidates);
  if not (List.is_empty candidates.Pagination.unvisible_right) then (
    Ui.draw_text ~colors ~focus:false ui_state ">"
  )

let rec shorten ui_state candidate s =
  if String.equal s "" ||
     Ui.text_width ui_state (candidate.display ^ s) <= X11.get_width () - 10 then
    s
  else
    shorten ui_state candidate
      (try "..." ^ String.sub s ~pos:10 ~len:(String.length s - 10)
       with _ -> "")

let draw_vertical { ui_state; colors; _ } extra_lines_geometry candidates =
  let candidates_with_geometry =
    Sequence.zip
      (Sequence.of_list extra_lines_geometry)
      (Pagination.visible candidates)
  in
  Sequence.iter ~f:(fun (line_geom, ((candidate, result), focus)) ->
    let cairo_ctx = Ui.cairo_ctx ui_state in
    Ui.set_x ui_state 0.;
    Ui.clear_line ~geometry:line_geom ~focus ~colors ui_state;
    Ui.draw_text_hl ~geometry:line_geom ~focus ~colors
      ui_state candidate.display result;
    if not (String.is_empty candidate.doc) then (
      let str = shorten ui_state candidate candidate.doc in
      let x = float @@ X11.get_width () - (Ui.text_width ui_state str) - 10 in
      Ui.set_x ui_state x;
      Ui.draw_text ~geometry:line_geom ~focus ~colors ui_state candidate.doc
    );
    Cairo.rel_move_to cairo_ctx 0. (float line_geom.height)
  ) candidates_with_geometry

let compute_geometry { ui_state; prompt; state; _ }: Ui.line_geometry list =
  let open State in
  let dstate = Ui.dstate ui_state in
  let main_line_geometry =
    let texts =
      List.join [
        [prompt];
        (List.map ~f:(fun (_, c) -> c.Candidate.display)
           state.State.entries);
        [state.before_cursor; state.after_cursor];
        (match state.layout with
         | MultiLine _ -> []
         | _ ->
           "<" :: ">" ::
           (Pagination.visible state.candidates
            |> Sequence.map ~f:(fun ((c, _), _) -> c.display)
            |> Sequence.to_list_rev));
      ]
    in
    let Draw.{ aligned_height; aligned_baseline; _ } =
      Draw.prepare_aligned_texts dstate texts in
    Ui.{ height = aligned_height; baseline = aligned_baseline }
  in
  Ui.update_bar_geometry ui_state main_line_geometry;

  let extra_lines_geometry =
    let extra_lines =
      match state.layout with
      | SingleLine | Grid (_, None) -> []
      | Grid (n, Some { pages; _ }) ->
        List.take pages.Pagination.visible n
      | MultiLine n ->
        List.take state.State.candidates.Pagination.visible n
    in
    List.map ~f:(fun (c, _) ->
      let Draw.{ aligned_height; aligned_baseline; _ } =
        Draw.prepare_aligned_texts dstate
          [c.Candidate.display; c.Candidate.doc] in
      Ui.{ height = aligned_height; baseline = aligned_baseline }
    ) extra_lines
  in
  extra_lines_geometry

let draw ({ ui_state; prompt; state; topbar; colors; _ } as app_state) =
  let open State in
  let extra_lines_geometry = compute_geometry app_state in
  let bar_geometry = Ui.current_bar_geometry ui_state in
  let total_height =
    bar_geometry.height +
    (List.fold_left ~init:0 ~f:(fun s { height; _ } -> s + height)
       extra_lines_geometry) in
  let dstate = Ui.dstate ui_state in
  let cairo_ctx = Draw.cairo_ctx dstate in

  Draw.render dstate ~height:total_height (fun () ->
    Draw.set_source_rgb dstate colors.window_background;
    Cairo.paint cairo_ctx;

    let oy = if topbar then 0. else float (total_height - bar_geometry.height) in
    Cairo.move_to cairo_ctx 0. oy;

    if not (String.is_empty prompt) then (
      Ui.draw_text ~colors ~focus:true ui_state prompt;
      incr ui_state
    );

    state.entries |> List.iter ~f:(fun (_, candidate) ->
      Ui.draw_text ~colors ~focus:false ui_state candidate.display;
      incr ui_state
    );

    Ui.draw_text ~colors ~focus:false ui_state
      (state.before_cursor ^ "|" ^ state.after_cursor);
    incr ui_state;

    let extra_lines_y = if topbar then float bar_geometry.height else 0. in
    begin match state.layout with
    | State.Grid (_, None)
    | State.SingleLine  -> draw_horizontal app_state

    | State.MultiLine _ ->
      Cairo.move_to cairo_ctx 0. extra_lines_y;
      draw_vertical app_state extra_lines_geometry state.candidates

    | State.Grid (_, Some { pages ; _ }) ->
      draw_horizontal app_state;
      Cairo.move_to cairo_ctx 0. extra_lines_y;
      draw_vertical app_state extra_lines_geometry pages
    end;
    ()
  );
  app_state

(* From Uutf (Uutf.Buffer.add_utf_8) *)
let utf8_of_uchar u =
  let b = Buffer.create 8 in
  let u = Uchar.to_scalar u in
  let w byte = Buffer.add_char b (Char.unsafe_of_int byte) in
  if u <= 0x007F then
    (w u)
  else if u <= 0x07FF then
    (w (0xC0 lor (u lsr 6));
     w (0x80 lor (u land 0x3F)))
  else if u <= 0xFFFF then
    (w (0xE0 lor (u lsr 12));
     w (0x80 lor ((u lsr 6) land 0x3F));
     w (0x80 lor (u land 0x3F)))
  else
    (w (0xF0 lor (u lsr 18));
     w (0x80 lor ((u lsr 12) land 0x3F));
     w (0x80 lor ((u lsr 6) land 0x3F));
     w (0x80 lor (u land 0x3F)));
  Buffer.contents b

let run_list ?(topbar = true) ?(separator = " ") ?(colors = Ui.Colors.default)
      ?(font = "DejaVu Sans Mono 9") ?(layout = State.SingleLine) ?(prompt = "")
      ?(hook = fun x -> x) program
  =
  let hook state =
    let state = hook state in
    { state with state = State.normalize state.state }
  in
  let dstate = Draw.init ~font ~topbar in
  let ui_state = Ui.make dstate in
  let state = {
    colors; prompt; font; topbar; hook; ui_state;
    state = State.initial ~layout ~separator ~program ~split:(splith ui_state);
  } in

  let rec loop state =
    let state = draw state in
    let loop_pure f = loop { state with state = f state.state } in
    let loop_transition f =
      let state', b = f state.state in
      let state' = { state with state = state' } in
      loop (if b then state'.hook state' else state')
    in

    let Key (ksym, mods, unicode) = X11.Events.wait () in
    if List.mem ~equal:Poly.(=) mods X11.Key.Control then (
      match ksym with
      | X11.Key.K_b -> loop_pure State.left
      | X11.Key.K_f -> loop_pure State.right
      | X11.Key.K_p -> loop_pure State.up
      | X11.Key.K_n -> loop_pure State.down
      | X11.Key.K_h -> loop_transition State.remove
      | _ -> loop state
    ) else if List.mem ~equal:Poly.(=) mods X11.Key.Mod1 then (
      match ksym with
      | X11.Key.K_h -> loop_pure State.left
      | X11.Key.K_j -> loop_pure State.down
      | X11.Key.K_k -> loop_pure State.up
      | X11.Key.K_l -> loop_pure State.right
      | _ -> loop state
    ) else (
      match ksym with
      | X11.Key.Escape -> Draw.terminate (Ui.dstate ui_state); []

      | X11.Key.Left  -> loop_pure State.left
      | X11.Key.Up    -> loop_pure State.up
      | X11.Key.Right -> loop_pure State.right
      | X11.Key.Down  -> loop_pure State.down

      | X11.Key.Scroll_up   -> loop_pure State.scroll_up
      | X11.Key.Scroll_down -> loop_pure State.scroll_down

      | X11.Key.Enter ->
        Draw.terminate (Ui.dstate ui_state); State.get_list state.state

      | X11.Key.Tab -> loop_transition State.complete

      | X11.Key.Backspace -> loop_transition State.remove

      | _ ->
        if Uchar.to_scalar unicode = 0 then
          loop state
        else (
          let s = utf8_of_uchar unicode in
          loop_transition (State.add_char s)
        )
    )
  in
  try loop state with e ->
    Draw.terminate (Ui.dstate ui_state);
    raise e

let run ?topbar ?separator ?colors ?font ?layout ?prompt ?hook program =
  match
    (run_list ?topbar ?separator ?colors ?font ?layout ?prompt ?hook program)
  with
  | [] -> None
  | l -> Some (String.concat ~sep:(Option.value ~default:" " separator) l)
