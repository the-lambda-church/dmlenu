(** Type of candidates *)

type t = <
  display: string;
  (** How to display the candidate to the user. *)
  real: string;
  (** What information it represent *)
  doc : string;
  (** Additional information that may be displayed to the user. *)
  completion: string;
  (** What should become of the inputbox after the user tried to
      complete on that one. *)
  matching_function: (string -> Matching.result option);
  (** How to know if the user's input matches this candidate *)
>
(** The type of candidates.
    
    A candidate is a possibility for completion or matching returned
    by sources. In dmenu, candidates were only strings but now we add
    more structure to differentiate the information and the way it
    should be matched, displayed to the user. *)

    
val make: ?real: string -> ?doc: string -> ?matching_function: (string -> Matching.result option) -> ?completion: string -> string -> t
(** [make ~real ~doc ~matching_function ~completion display]
    creates a new candidate. The default values are:
    - real, completion: display
    - doc: empty
    - matching_function: Matching.match_query ~candidate
*)
