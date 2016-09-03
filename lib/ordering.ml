open Batteries

type matched_candidate = Candidate.t * Matching.result
type t = matched_candidate list -> matched_candidate list

let prefixes_first =
  List.partition (function
    | (_, [(true, _, _); (false, _, _)]) -> true
    |  _                                 -> false)
  %> uncurry (@)

let reorder_matched_fun = ref identity
let reorder_matched l = !reorder_matched_fun l
let set_reorder_matched_fun f = reorder_matched_fun := f
