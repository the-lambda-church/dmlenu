(* This file deals with matching substring *)
open Batteries

type result = ((bool * int * int) list)

let match_query ~case ~query ~candidate = 
  let query, candidate = 
    if case then 
      query, candidate
    else
      String.lowercase query, String.lowercase candidate
  in
  try
    let k = String.find candidate query in
    Some [
      (false, 0, k) ;
      (true, k, k + String.length query) ;
      (false, k + String.length query, String.length candidate) ;
    ]
  with _ -> None
  
