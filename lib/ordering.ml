open Batteries

type matched_candidate = Candidate.t * Matching.result
type t = matched_candidate list -> matched_candidate list

let prefixes_first l =
  let prefixes, other =
    List.partition (function
      | (_, [(true, _, _); (false, _, _)]) -> true
      | (_, [(true, _, _)])                -> true
      |  _                                 -> false) l
  in
  let full_matchs, strict_prefixes =
    List.partition (function
      | (_, [(true, _, _)]) -> true
      | _                   -> false) prefixes
  in
  full_matchs @ strict_prefixes @ other

let reorder_matched_fun = ref identity
let reorder_matched l = !reorder_matched_fun l
let set_reorder_matched_fun f = reorder_matched_fun := f
