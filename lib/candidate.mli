(** Type of candidates *)

type t = {
  display: string;
  (** How to display the candidate to the user. *)
  real: string;
  (** What information it represent *)
  doc : string;
  (** Additional information that may be displayed to the user. *)
  completion: string;
  (** What should become of the inputbox after the user tried to
      complete on that one. *)
  matching_function: Matching.t
  (** How to know if the user's input matches this candidate *)
}
(** The type of candidates.
    
    A candidate is a possibility for completion or matching returned
    by sources. In dmenu, candidates were only strings but now we add
    more structure to differentiate the information and the way it
    should be matched, displayed to the user. *)

    
val make : ?real:string -> ?doc:string -> ?matching_function:Matching.t ->
  ?completion:string -> string -> t
(** [make ~real ~doc ~matching_function ~completion display]
    creates a new candidate. The default values are:
    - real, completion: display
    - doc: empty
    - matching_function: Matching.match_query ~candidate
*)

(** {3 Reordering functions} *)

(** After matching on candidates, a reordering function is called that can
    change their order. *)

val reorder_matched : (t * Matching.result) list -> (t * Matching.result) list
(** The reordering function (by default, the identity). *)

val set_reorder_matched_fun : ((t * Matching.result) list -> (t * Matching.result) list) -> unit
(** Set the reordering function. *)

(** {2 Predefined reordering functions} *)

(** Reorder the candidates to put prefix matchings first. *)
val prefixes_first : (t * Matching.result) list -> (t * Matching.result) list
