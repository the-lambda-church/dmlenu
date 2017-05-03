open Base

type t = {
  display: string;
  real: string;
  doc : string;
  completion: string;
  matching_function: Matching.t;
}

let make ?real ?(doc = "") ?matching_function ?completion display : t = {
  display; doc;
  real = Option.value ~default:display real;
  completion = Option.value ~default:display completion;
  matching_function =
    Option.value ~default:(Matching.match_query ~candidate: display) matching_function;
}
