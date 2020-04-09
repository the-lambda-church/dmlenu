open! Backend

let two_pi = 2. *. acos(-1.)

type prepared_text = {
  txt : string;
  layout : Pango.layout;
  extent : Pango.rectangle;
  baseline : int;
}

let prepare_text
      (cr: Cairo.context) (pc: Pango.context)
      (font: Pango.font_description) (txt: string)
  =
  let layout = Pango.Layout.create pc in
  Pango.Layout.set_font_description layout font;
  Pango.Layout.set_text layout txt;
  Pango.Layout.set_single_paragraph_mode layout true;
  Cairo_pango.update_layout cr layout;
  let extent = Pango.Layout.get_pixel_extent layout in
  let baseline = Pango.Layout.get_baseline layout / Pango.scale (* ? *) in
  { txt; layout; extent; baseline }

let draw_text (cr: Cairo.context) =
  let font_map = Cairo_pango.Font_map.get_default() in
  let pc = Cairo_pango.Font_map.create_context font_map in
  (* Cairo_pango.set_resolution pc 161.; *)
  let font = Pango.Font.from_string "monospace 10" in

  let prepared = prepare_text cr pc font "Hello world こんにちは世界 🤔" in
  let height = prepared.extent.height in
  X11.resize_height height;
  Printf.printf "x: %d; y: %d; width: %d; height: %d\n%!"
    prepared.extent.x
    prepared.extent.y
    prepared.extent.width
    prepared.extent.height;
  Printf.printf "baseline: %d\n%!" prepared.baseline;

  Cairo.Group.push cr;
  Cairo.set_source_rgb cr 1. 0. 0.;
  Cairo.rectangle cr 0.5 0.5 ~w:20. ~h:(float height -. 0.5);
  Cairo.stroke cr;
  Cairo.Group.pop_to_source cr;
  Cairo.paint cr;

  Cairo.translate cr (-. float prepared.extent.x) (-. float prepared.extent.y);
  Cairo.set_source_rgb cr 1. 1. 1.;
  Cairo_pango.show_layout cr prepared.layout;
  Cairo.set_line_width cr 1.;
  ()

let draw_square cr =
  Cairo.Group.push cr;
  Cairo.set_source_rgb cr 1. 0. 0.;
  Cairo.rectangle cr 0.5 0.5 ~w:20. ~h:(float 20 -. 0.5);
  Cairo.stroke cr;
  Cairo.Group.pop_to_source cr;
  Cairo.paint cr;
  ()

let () =
  let dstate = Draw.init ~font:"monospace 10" ~topbar:true in
  let cr = dstate.cairo in

  let Draw.{ aligned_height; aligned_baseline; aligned_texts } =
    Draw.prepare_aligned_texts dstate [
      "aec";
      "Hello world";
      "Hello world こんにちは世界 🤔";
      "hello 🐦🤔";
      "Hello world こんにちは世界";
    ]
  in
  Printf.printf "max height: %d\n%!" aligned_height;

  Draw.render dstate aligned_height (fun () ->
    Cairo.set_source_rgb cr 0. 0. 0.;
    Cairo.paint cr;
    Cairo.set_line_width cr 1.;

    Cairo.set_source_rgb cr 1. 1. 1.;

    let x = ref 0. in
    List.iter (fun (txt: Draw.prepared_text) ->
      let offset_y = aligned_baseline - txt.baseline in
      Printf.printf "%s: offset_y: %d, width: %d\n%!"
        txt.Draw.text offset_y txt.Draw.width;

      Cairo.move_to cr !x (float offset_y);
      Cairo_pango.show_layout cr txt.layout;
      x := !x +. float txt.Draw.width +. 10.;
    ) aligned_texts;

    Draw.draw_sharp_rectangle cr ~color:(255., 0., 0.)
      ~x:0. ~y:0. ~w:!x ~h:(float aligned_height);
    Draw.draw_sharp_filled_rectangle cr ~color:(0., 255., 0.)
      ~x:(!x -. 20.) ~y:0. ~w:20. ~h:(float aligned_height);

    x := !x +. 10.;
    let _print txt =
      Cairo.move_to cr !x 0.;
      let t = Draw.prepare_text dstate txt in
      Cairo_pango.show_layout cr t.layout;
      x := !x +. float t.width;
      Cairo.move_to cr !x 0.;
    in
    (* let print_hl txt matching =
     *   Cairo.move_to cr !x 0.;
     *   let txt =
     *     Draw.markup_of_matching_result ~color:"#ff0000" ~text:txt matching in
     *   (\* let txt = "<span background='#0000ff'>" ^ txt ^ "</span>" in *\)
     *   let t = Draw.prepare_text ~markup:true dstate txt in
     *   Cairo_pango.show_layout cr t.layout;
     *   x := !x +. float t.width;
     *   Cairo.move_to cr !x 0.;
     * in *)

    x := !x +. 10.;
    Cairo.move_to cr !x 0.;

    let xoff =
      let Draw.{ height; _ } = Draw.prepare_text dstate "" in
      Printf.printf "xoff: %d\n%!" (height / 2);
      height / 2
    in

    Draw.draw_text_hl ~color_background:(0., 1., 1.) ~height:aligned_height
      ~color_foreground:(1., 1., 1.)
      ~color_hl:(1., 0., 0.)
      ~xoff
      ~baseline:aligned_baseline ~state:dstate "abcdefghi"
      [(false, 0, 3); (true, 3, 6); (false, 6, 9)];

    Draw.draw_text ~color_background:(0., 0., 1.) ~height:aligned_height
      ~color_foreground:(1., 1., 1.)
      ~xoff
      ~baseline:aligned_baseline ~state:dstate "abcdefghi";

    Cairo.rel_move_to dstate.cairo 5. 0.;

    Draw.draw_text ~color_background:(0., 0., 1.) ~height:aligned_height
      ~color_foreground:(1., 1., 1.)
      ~xoff
      ~baseline:aligned_baseline ~state:dstate "foobar";

    (* print "abcdefghi"; *)

    ()
  );

  let rec loop () =
    let (ksym, _mods, unicode) = X11.next_key_event () in
    if ksym = 0xff1b then ()
    else (
      let u = Uchar.to_int unicode in
      Printf.printf "key press: sym:%x ucode:%x %s\n%!" ksym u
        (try String.make 1 (Uchar.to_char unicode) with
           Invalid_argument _ -> "");
      loop ()
    ) 
  in

  loop ();

  X11.terminate ()
