open Batteries

type t = {
  display: string;
  real: string;
  doc : string;
  completion: string;
  matching_function: Matching.t;
}

let make ?real ?(doc = "") ?matching_function ?completion display : t = {
  display; doc;
  real = Option.default display real;
  completion = Option.default display completion;
  matching_function = 
    Option.default (Matching.match_query ~candidate: display) matching_function;
}

(* ************************************************************************** *)

let prefixes_first =
  List.partition (function
    | (_, [(true, _, _); (false, _, _)]) -> true
    |  _                                 -> false)
  %> uncurry (@)

let reorder_matched_fun = ref identity
let reorder_matched l = !reorder_matched_fun l
let set_reorder_matched_fun f = reorder_matched_fun := f
