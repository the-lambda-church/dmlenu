(** Functions used to (re)order the list of candidates after matching. *)

(** State transition functions (defined in {!State}, used by {!Dmlenu.run}) use
    the default {!reorder_matched} reordering function, that you can change
    using {!set_reorder_matched_fun}. *)

type matched_candidate = Candidate.t * Matching.result
(** A candidate that has been successfully matched. *)

type t = matched_candidate list -> matched_candidate list
(** The type of ordering functions *)

(** {3 Reordering function} *)

val reorder_matched : t
(** The default reordering function: by default, the identity *)

val set_reorder_matched_fun : t -> unit
(** Set the default reordering function *)

(** {2 Predefined reordering functions} *)

(** Reorder the candidates to put prefix matchings first. *)
val prefixes_first : t
