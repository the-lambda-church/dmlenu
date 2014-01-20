open Batteries

type t = {
  display: string;
  real: string;
  doc : string;
  completion: string;
  matching_function: (string -> Matching.result option);
}

let make ?real ?(doc = "") ?matching_function ?completion display : t= 
  { display; doc;
    real = Option.default display real;
    completion = Option.default display completion;
    matching_function = 
      Option.default (Matching.match_query ~candidate: display) matching_function;
  }
