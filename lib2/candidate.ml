type t = <
  display: string;
  real: string;
  doc : string;
  completion: string;
  matching_function: (string -> Matching.result option);
>
