(* This file deals with matching substring *)
open Batteries

type result = ((bool * int * int) list)


let make_list candidate list = 
  let list, old = 
    List.fold_left (fun (list, old) (start', stop') ->
      (true, start', stop') :: (false, old, start') :: list, stop'
    ) ([], 0) list
  in
  let list = List.filter (fun (_, k, k') -> k <> k') 
    (List.rev ((false, old, String.length candidate) :: list)) in
  list

let match_query ~case ~query ~candidate = 
  let query, candidate = 
    if case then 
      query, candidate
    else
      String.lowercase query, String.lowercase candidate
  in
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
  

    
