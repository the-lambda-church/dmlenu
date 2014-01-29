type t = {
  sources: Source.t list; 
  transition : Candidate.t -> t; 
}


let rec empty = {
  sources = [] ;
  transition = (fun _ -> empty);
}

let rec iterate sources = {
  sources;
  transition = fun _ -> iterate sources ;
}

let singleton source = { empty with sources = [ source ] }

let rec concat a b =
  if a.sources = [] then
    b
  else
    { a with transition = fun o -> concat (a.transition o) b }

let sum source f = {
  sources = [source];
  transition = fun o -> f o 
}

let csum l =
  let src = Source.from_list_ @@ List.map fst l in
  sum src (fun o -> Lazy.force (List.assoc o.Candidate.display l))
