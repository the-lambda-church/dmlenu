(* This file deals with matching substring *)
open Batteries

type result = ((bool * int * int) list)

let handle_case case_sensitive query candidate =
  if case_sensitive then
    query, candidate
  else
    String.lowercase query, String.lowercase candidate

let make_list candidate list =
  let list, old =
    List.fold_left (fun (list, old) (start', stop') ->
      (true, start', stop') :: (false, old, start') :: list, stop'
    ) ([], 0) list
  in
  let list = List.filter (fun (_, k, k') -> k <> k')
    (List.rev ((false, old, String.length candidate) :: list)) in
  list

let subset ?(case=true) ~candidate query =
  let query, candidate = handle_case case query candidate in
  try
    let words = List.filter ((<>) "") (String.nsplit query " ") in
    let matches = List.map (fun word ->
      let n = String.find candidate word in
      (n, n + String.length word)) words
    in
    Some
      (make_list candidate
         (List.sort (fun x y -> compare (fst x) (fst y)) matches))
  with Not_found -> None

let partial_match ?(case=true) ~candidate query =
  let query, candidate = handle_case case query candidate in
  try
    let k = String.find candidate query in
    Some [
      (false, 0, k) ;
      (true, k, k + String.length query) ;
      (false, k + String.length query, String.length candidate) ;
    ]
  with _ -> None

let match_prefix ?(case=true) ~candidate query =
  let query, candidate = handle_case case query candidate in
  if not (String.starts_with candidate query) then None else
  let qlen = String.length query in
  Some [ (true, 0, qlen) ; (false, qlen, String.length candidate) ]

let fuzzy_match ?(case=true) ~candidate query =
  let query, candidate = handle_case case query candidate in
  let find_char (lst, offset, rest) c =
    let skipped, rest = String.split rest ~by:(String.of_char c) in
    let offset' = offset + String.length skipped in
    let lst' =
      (true, offset', offset' + 1) ::
      (false, offset, offset') ::
      lst
    in
    (lst', offset' + 1, rest)
  in
  try
    let (lst,offset,_) = String.fold_left find_char ([], 0, candidate) query in
    Some (List.rev @@ (false, offset, String.length candidate) :: lst)
  with Not_found ->
    None

let fuzzy_prefix ?(case=true) ~candidate query =
  let query, candidate = handle_case case query candidate in
  if query <> "" && candidate <> "" && candidate.[0] <> query.[0] then None else
  fuzzy_match ~case ~candidate query

let trivial s' _ = Some [false, 0, String.length s']

(* ************************************************************************** *)
let default_match_fun = ref (match_prefix ~case:true)

let set_match_query_fun f = default_match_fun := f

let match_query ~candidate query = !default_match_fun ~candidate query

