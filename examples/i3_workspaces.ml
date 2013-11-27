let prompt = try Sys.argv.(1) with Invalid_argument _ -> ""

let run =
  let open Dmlenu in
  let app_state = {
    prompt ;
    compl = Completion.make_state [ Extra_sources.i3_workspaces () ] ;
  }
  in
  run app_state default_conf
