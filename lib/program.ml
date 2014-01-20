type t = {
  sources: Source.t list; 
  transition : Candidate.t -> t; 
  completion: Source.t list; 
   (** The sources used to actually complete the display fields of the sources.
       It is only when the lines mode is set. *)
}


let rec empty = {
  sources = [] ;
  transition = (fun _ -> empty);
  completion = [];
}

let rec iterate ?(completion = []) sources = {
  sources ;
  completion = [];
  transition = fun _ -> iterate sources ;
    
}

let singleton ?(completion = []) source = { empty with sources = [ source ]; completion }

let rec concat a b =
  if a.sources = [] then
    b
  else
    { a with transition = fun o -> concat (a.transition o) b }

let sum source f =
  { sources = [source];
    completion = [];
    transition = fun o -> f o }

let csum l =
  let src = Source.from_list_ @@ List.map fst l in
  sum src (fun o -> Lazy.force (List.assoc o.Candidate.display l))
