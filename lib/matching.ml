(* This file deals with matching substring *)
open Base

type result = ((bool * int * int) list)
type t = string -> result option

let handle_case case_sensitive query candidate =
  if case_sensitive then
    query, candidate
  else
    String.lowercase query, String.lowercase candidate

let make_list candidate list =
  let list, old =
    List.fold ~f:(fun (list, old) (start', stop') ->
      (true, start', stop') :: (false, old, start') :: list, stop'
    ) ~init:([], 0) list
  in
  let list = List.filter ~f:(fun (_, k, k') -> k <> k')
    (List.rev ((false, old, String.length candidate) :: list)) in
  list

let subset ?(case=true) ~candidate query =
  let query, candidate = handle_case case query candidate in
  let opt_value_exn = function Some x -> x | None -> raise Caml.Not_found in
  try
    let words =
      List.filter ~f:(Fn.non String.is_empty) (String.split query ~on:' ') in
    let matches = List.map ~f:(fun word ->
      let n = String.substr_index candidate ~pattern:word |> opt_value_exn in
      (n, n + String.length word)) words
    in
    Some
      (make_list candidate
         (List.sort ~compare:(fun x y -> compare (fst x) (fst y)) matches))
  with Caml.Not_found -> None

let partial_match ?(case=true) ~candidate query =
  let query, candidate = handle_case case query candidate in
  Option.map (String.substr_index candidate ~pattern:query) ~f:(fun k ->
    [
      (false, 0, k) ;
      (true, k, k + String.length query) ;
      (false, k + String.length query, String.length candidate) ;
    ]
  )

let match_prefix ?(case=true) ~candidate query =
  let query, candidate = handle_case case query candidate in
  if not (String.is_prefix candidate ~prefix:query) then None else
  let qlen = String.length query in
  Some [ (true, 0, qlen) ; (false, qlen, String.length candidate) ]

let fuzzy_match ?(case=true) ~candidate query =
  let query, candidate = handle_case case query candidate in
  let find_char (lst, offset, rest) c =
    let skipped, rest = String.lsplit2_exn rest ~on:c in
    let offset' = offset + String.length skipped in
    let lst' =
      (true, offset', offset' + 1) ::
      (false, offset, offset') ::
      lst
    in
    (lst', offset' + 1, rest)
  in
  try
    let (lst,offset,_) = String.fold ~f:find_char ~init:([], 0, candidate) query in
    Some (List.rev ((false, offset, String.length candidate) :: lst))
  with Caml.Not_found | Not_found_s _ ->
    None

let fuzzy_prefix ?(case=true) ~candidate query =
  let query, candidate = handle_case case query candidate in
  if not (String.is_empty query) &&
     not (String.is_empty candidate) &&
     Char.(candidate.[0] <> query.[0])
  then None
  else fuzzy_match ~case ~candidate query

let trivial ~candidate _ = Some [false, 0, String.length candidate]

(* ************************************************************************** *)
let default_match_fun = ref (match_prefix ~case:true)

let set_match_query_fun f = default_match_fun := f

let match_query ~candidate query = !default_match_fun ~candidate query

let on f t = fun s -> t (f s)
