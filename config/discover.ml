open Base
open Stdio
module C = Configurator.V1

let write_sexp fn sexp =
  Out_channel.write_all fn ~data:(Sexp.to_string sexp)

let libs = ref []
let cflags = ref []

let () =
  let handle_arg s =
    let dir = Caml.Filename.dirname s in
    cflags := [Printf.sprintf "-I%s" dir]
  in

  let discover_pkg pc name =
    let conf =
      match (C.Pkg_config.query pc ~package:name) with
      | None ->
        eprintf "Error: no pkg-config entry found for %s\n" name;
        Caml.exit 1
      | Some pkg -> pkg
    in
    libs := conf.libs @ !libs;
    cflags := conf.cflags @ !cflags;
  in

  let main c =
    let pkgconfig =
      match C.Pkg_config.get c with
      | None ->
        eprintf "Error: could not find pkg-config\n";
        Caml.exit 1
      | Some pc -> pc
    in
    List.iter ~f:(discover_pkg pkgconfig) [
      "x11";
      "xinerama";
      "cairo-xlib";
    ];
    write_sexp "cclib.sexp" (sexp_of_list sexp_of_string !libs);
    write_sexp "ccopt.sexp" (sexp_of_list sexp_of_string !cflags);
  in

  C.main
    ~args:["--cairo2", Caml.Arg.String handle_arg,
           "ocaml cairo2 bindings .h location"]
    ~name:"discover"
    main
