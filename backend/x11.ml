external init : bool -> int -> bool = "caml_x11_init"
external terminate : unit -> unit = "caml_x11_terminate"
external resize_height : int -> unit = "caml_x11_resize_height"
external flush : unit -> unit = "caml_x11_flush"
external get_cairo_surface : unit -> Cairo.Surface.t = "caml_x11_get_cairo_surface"
external get_width : unit -> int = "caml_x11_get_width"
external get_height : unit -> int = "caml_x11_get_height"
external next_key_event : unit -> int * int * Uchar.t = "caml_x11_next_key_event"

module Key = struct
  type keysym =
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
    | K_b
    | K_f
    | K_p
    | K_n
    | K_h
    | K_j
    | K_k
    | K_l
    | Other

  let keysym_of_int = function
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
    | 0x0062 -> K_b
    | 0x0066 -> K_f
    | 0x0070 -> K_p
    | 0x006e -> K_n
    | 0x0068 -> K_h
    | 0x006a -> K_j
    | 0x006b -> K_k
    | 0x006c -> K_l
    | _ -> Other

  let shiftMask = 1 lsl 0
  let lockMask = 1 lsl 1
  let controlMask = 1 lsl 2
  let mod1Mask = 1 lsl 3
  let mod2Mask = 1 lsl 4
  let mod3Mask = 1 lsl 5
  let mod4Mask = 1 lsl 6
  let mod5Mask = 1 lsl 7

  type modifier =
    | Shift
    | Lock
    | Control
    | Mod1
    | Mod2
    | Mod3
    | Mod4
    | Mod5

    let mods_of_int x =
    let mods = ref [] in
    let add_mod m z = if x land m <> 0 then mods := z :: !mods in
    add_mod shiftMask Shift;
    add_mod lockMask Lock;
    add_mod controlMask Control;
    add_mod mod1Mask Mod1;
    add_mod mod2Mask Mod2;
    add_mod mod3Mask Mod3;
    add_mod mod4Mask Mod4;
    add_mod mod5Mask Mod5;
    !mods
end
