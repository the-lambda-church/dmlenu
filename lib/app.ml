open Base
open Candidate
open Backend

type app_state = {
  colors: X.Colors.t; (** The set of colors to use *)
  font: string;
  prompt: string; (** The prompt to the user *)
  topbar: bool;  (** Shall dmlenu sit on the bottom or on the top of the screen? *)
  hook: (app_state -> app_state); (** Hook called whenever a token is added *)
  state: State.t; (** The state of the current engine *)
  dstate: Draw.state;
}

let float = Float.of_int

let text_width dstate txt =
  let text_size = Draw.prepare_text dstate txt in
  text_size.Draw.width

let splith dstate list =
  let rec go index = function
    | [] -> [], []
    | (candidate, rest) :: q as l->
      let size = text_width dstate candidate.display + 10 in
      if (index + size) > X11.get_width () - 5 then [], l else
      let a, b = go (index+size) q in
      (candidate, rest) :: a, b
  in
  go 0 list

let incr ~dstate = Cairo.rel_move_to dstate.Draw.cairo 5. 0.

let compute_xoff ~dstate =
  let Draw.{ height; _ } = Draw.prepare_text dstate "" in
  height / 2

let text_foreground ~focus (st: app_state) =
  if focus then
    (* XXX *)
    Draw.parse_color_exn st.colors.focus_foreground
  else
    Draw.parse_color_exn st.colors.normal_foreground

let text_background ~focus (st: app_state) =
  if focus then
    Draw.parse_color_exn st.colors.focus_background
  else
    Draw.parse_color_exn st.colors.normal_background

let draw_horizontal ~xoff ({ dstate; state; _ } as st) bar_geometry  =
  let open State in
  let candidates = state.candidates in
  let draw_text ~focus txt =
    Draw.draw_text ~markup:false ~xoff
      ~color_background:(text_background ~focus st)
      ~color_foreground:(text_foreground ~focus st)
      ~height:bar_geometry.height ~baseline:bar_geometry.baseline
      ~state:dstate txt
  in
  let draw_text_hl ~focus txt matching_result =
    Draw.draw_text_hl ~markup:false ~xoff
      ~color_background:(text_background ~focus st)
      ~color_foreground:(text_foreground ~focus st)
      ~height:bar_geometry.height ~baseline:bar_geometry.baseline
      ~state:dstate txt matching_result
  in

  if not (List.is_empty candidates.Pagination.unvisible_left) then (
    draw_text ~focus:false "<";
    incr ~dstate
  );
  Pagination.fold_visible (fun () visible (candidate, result) ->
    draw_text_hl ~focus:visible
      ~color_hl:(Draw.parse_color_exn st.colors.match_foreground)
      candidate.display result;
    incr ~dstate
  ) () candidates;
  if not (List.is_empty candidates.Pagination.unvisible_right) then (
    draw_text ~focus:false ">"
  )

let rec shorten ~dstate candidate s =
  if text_width dstate (candidate.display ^ s) <= X11.get_width () - 10 then
    s
  else
    shorten ~dstate candidate
      (try "..." ^ String.sub s ~pos:10 ~len:(String.length s - 10)
       with _ -> "")

let draw_vertical ~xoff ({ dstate; _} as st)
      extra_lines_geometry candidates =

  Pagination.fold_visible (fun lines_geom focus (candidate, result) ->
    (* XXX *)
    let (line_geom: State.line_geometry), lines_geom_tl =
      match lines_geom with
      | x :: xs -> x, xs
      | [] -> assert false
    in
    let _, y = Cairo.Path.get_current_point dstate.cairo in
    if focus then
      Draw.draw_sharp_filled_rectangle dstate.cairo
        ~color:(Draw.parse_color_exn st.colors.focus_background)
        ~x:0. ~y ~w:(float (X11.get_width ())) ~h:(float line_geom.height);
    Cairo.move_to dstate.cairo 0. y;
    Draw.draw_text_hl ~markup:false ~xoff
      ~color_background:(text_background ~focus st)
      ~color_foreground:(text_foreground ~focus st)
      ~color_hl:(Draw.parse_color_exn st.colors.match_foreground)
      ~height:line_geom.height ~baseline:line_geom.baseline
      ~state:dstate candidate.display result;
    if not (String.is_empty candidate.doc) then (
      let str = shorten ~dstate candidate candidate.doc in
      let x = float @@ X11.get_width () - (text_width dstate str) - 10 in
      Cairo.move_to dstate.cairo x y;
      Draw.draw_text ~xoff
        ~color_background:(text_background ~focus st)
        ~color_foreground:(text_foreground ~focus st)
        ~height:line_geom.height ~baseline:line_geom.baseline
        ~state:dstate candidate.doc
    );
    Cairo.move_to dstate.cairo 0. (y +. float line_geom.height);
    lines_geom_tl

    (* X.Draw.clear_line line (X.colors xstate).X.Colors.focus_background ;
     * X.Draw.text_hl ~state: xstate ~focus ~result candidate.display;
     * if not (String.is_empty candidate.doc) then (
     *   let str = shorten ~state: xstate candidate candidate.doc in
     *   let x = X.width ~state: xstate - (X.text_width ~state: xstate str) - 10 in
     *   let () = X.Draw.set_x ~state: xstate ~x in
     *   X.Draw.text ~focus ~state: xstate "%s" candidate.doc
     * ) *)
  ) extra_lines_geometry candidates
  |> ignore

(* let resize =
 *   let current = ref 0 in
 *   fun ~state ~lines ->
 *     if !current <> lines then (
 *       current := lines ;
 *       X.resize ~state ~lines
 *     ) *)

let compute_geometry ({ dstate; prompt; state; _ } as app_state):
  app_state * State.line_geometry list
  =
  let open State in
  let main_line_geometry =
    let texts =
      List.join [
        [prompt];
        (List.map ~f:(fun (_, c) -> c.Candidate.display)
           state.State.entries);
        [state.before_cursor; state.after_cursor];
        (match app_state.state.layout with
         | MultiLine _ -> []
         | _ ->
           "<" :: ">" ::
           Pagination.fold_visible (fun txts _ (c, _) -> c.display :: txts)
             [] state.candidates);
      ]
    in
    let Draw.{ aligned_height; aligned_baseline; _ } =
      Draw.prepare_aligned_texts dstate texts in
    if state.bar_geometry.height >= aligned_height then
      state.bar_geometry
    else
      { height = aligned_height; baseline = aligned_baseline }
  in

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
      { height = aligned_height; baseline = aligned_baseline }
    ) extra_lines
  in

  { app_state with state = { state with bar_geometry = main_line_geometry } },
  extra_lines_geometry

let draw ({ dstate; prompt; state; topbar; _ } as app_state) =
  let open State in
  let app_state, extra_lines_geometry = compute_geometry app_state in
  let bar_geometry =
    (* XX *)
    let voff = 2 in
    { height = app_state.state.bar_geometry.height + 2 * voff;
      baseline = app_state.state.bar_geometry.baseline + voff }
  in
  let total_height =
    bar_geometry.height +
    (List.fold_left ~init:0 ~f:(fun s { height; _ } -> s + height)
       extra_lines_geometry)
  in
  let xoff = compute_xoff ~dstate in
  let draw_bar_text ~focus text =
    Draw.draw_text
      ~color_background:(text_background ~focus app_state)
      ~color_foreground:(text_foreground ~focus app_state)
      ~xoff ~height:bar_geometry.height ~baseline:bar_geometry.baseline
      ~state:dstate text
  in

  Draw.render dstate total_height (fun () ->
    begin
      let r, g, b =
        Draw.parse_color_exn app_state.colors.window_background in
      Cairo.set_source_rgb dstate.cairo r g b
    end;
    Cairo.paint dstate.cairo;

    let oy = if topbar then 0. else float (total_height - bar_geometry.height) in
    Cairo.move_to dstate.cairo 0. oy;

    if not (String.is_empty prompt) then (
      draw_bar_text ~focus:true prompt;
      incr ~dstate
    );

    state.State.entries |> List.iter ~f:(fun (_, candidate) ->
      draw_bar_text ~focus:false candidate.display;
      incr ~dstate
    );

    draw_bar_text ~focus:false (state.before_cursor ^ "|" ^ state.after_cursor);
    incr ~dstate;

    let extra_lines_y = if topbar then float bar_geometry.height else 0. in
    begin match app_state.state.State.layout with
    | State.Grid (_, None)
    | State.SingleLine  -> draw_horizontal ~xoff app_state bar_geometry

    | State.MultiLine _ ->
      Cairo.move_to dstate.cairo 0. extra_lines_y;
      draw_vertical ~xoff app_state extra_lines_geometry
        app_state.state.State.candidates

    | State.Grid (_, Some { pages ; _ }) ->
      draw_horizontal ~xoff app_state bar_geometry;
      Cairo.move_to dstate.cairo 0. extra_lines_y;
      draw_vertical ~xoff app_state extra_lines_geometry pages
    end;
    ()
  );
  app_state


let run_list ?(topbar = true) ?(separator = " ") ?(colors = X.Colors.default)
      ?(font = "DejaVu Sans Mono 9") ?(layout = State.SingleLine) ?(prompt = "")
      ?(hook = fun x -> x) program
  =
  let hook state =
    let state = hook state in
    { state with state = State.normalize state.state }
  in
  let dstate = Draw.init ~font ~topbar in
  let state = {
    colors; prompt; font; topbar; hook; dstate;
    state = State.initial ~layout ~separator ~program ~split:(splith dstate);
  } in

  let rec loop state =
    let state = draw state in
    let loop_pure f = loop { state with state = f state.state } in
    let loop_transition f =
      let state', b = f state.state in
      let state' = { state with state = state' } in
      loop (if b then state'.hook state' else state')
    in

    (* let input_text s =
     *   if not (String.is_empty s) then loop_transition (State.add_char s)
     *   else loop state
     * in *)

    let ksym, mods, unicode = X11.next_key_event () in
    let ksym = X11.Key.keysym_of_int ksym in
    let mods = X11.Key.mods_of_int mods in
    let utf8_of_uchar u =
      let b = Buffer.create 8 in
      Uutf.Buffer.add_utf_8 b u;
      Buffer.contents b
    in
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
      | X11.Key.Escape -> Draw.terminate state.dstate; []

      | X11.Key.Left  -> loop_pure State.left
      | X11.Key.Up    -> loop_pure State.up
      | X11.Key.Right -> loop_pure State.right
      | X11.Key.Down  -> loop_pure State.down

      | X11.Key.Scroll_up   -> loop_pure State.scroll_up
      | X11.Key.Scroll_down -> loop_pure State.scroll_down

      | X11.Key.Enter ->
        Draw.terminate state.dstate; State.get_list state.state

      | X11.Key.Tab -> loop_transition State.complete

      | X11.Key.Backspace -> loop_transition State.remove

      | _ ->
        if Uchar.to_scalar unicode = 0 then
          loop state
        else (
          let s = utf8_of_uchar unicode in
          (* let s =
           *   if String.equal s "z" then
           *     "は世界"
           *   else if String.equal s "w" then
           *     "こんにち"
           *   else if String.equal s "j" then
           *     "🤔"
           *   else s
           * in *)
          loop_transition (State.add_char s)
        )
    )
  in
  try loop state with e ->
    Draw.terminate state.dstate;
    raise e

let run ?topbar ?separator ?colors ?layout ?prompt ?hook program =
  match
    (run_list ?topbar ?separator ?colors ?layout ?prompt ?hook program)
  with
  | [] -> None
  | l -> Some (String.concat ~sep:(Option.value ~default:" " separator) l)
