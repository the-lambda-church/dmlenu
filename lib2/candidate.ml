open Batteries

type t = <
  display: string;
  real: string;
  doc : string;
  completion: string;
  matching_function: (string -> Matching.result option);
>

let make ?real ?(doc = "") ?matching_function ?completion display : t= 
  object
    method display = display
    method doc = doc
    method real = Option.default display real
    method completion = Option.default display completion
    method matching_function = Matching.match_query ~candidate: display
  end
