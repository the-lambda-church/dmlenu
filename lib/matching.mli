type result = ((bool * int * int) list)
(** The type of return of matching functions.  If it succeeded Some l,
    a list of terms (match, start, stop) that denotes substring of the
    candidates that are matched or not. *)


(** {3 Matching function} *)
val match_query : candidate:string -> string -> result option

val set_match_query_fun : (candidate:string -> string -> result option) -> unit

(** {2 Predefined matching functions} *)

(** NB: for the following function, the default value of [case] is [true]. *)

val asmanur_match_query : ?case:bool -> candidate:string -> string -> result option
(** [asmanur_match_query ?case ~candidate query] returns the result of
    matching [candidate] against [query]. If [case] is true then the
    matching is case sensitive.

    TODO(asmanur): rename + document the matching algorithm. *)

val partial_match : ?case:bool -> candidate:string -> string -> result option
(** [partial_match ?case ~candidate sub] will return [Some _] if [sub] is a
    substring of [candidate], [None] otherwise.. *)

val match_prefix : ?case:bool -> candidate:string -> string -> result option
