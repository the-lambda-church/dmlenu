(** Functions used to match candidates. *)

type result = ((bool * int * int) list)
(** The return type of matching functions.  If it succeeded Some l, a list of
    terms (match, start, stop) that denotes substring of the candidates that are
    matched or not. *)

(** {3 Matching function} *)

val match_query : candidate:string -> string -> result option

val set_match_query_fun : (candidate:string -> string -> result option) -> unit

(** {2 Predefined matching functions} *)

(** NB: for the following functions the default value of [case] is [true]. *)

val subset : ?case:bool -> candidate:string -> string -> result option
(** [subset ?case ~candidate query] will match if query (interpreded as a set of
    characters) is a subset of candidate (interpreted as a set of chars again).
*)

val partial_match : ?case:bool -> candidate:string -> string -> result option
(** [partial_match ?case ~candidate sub] will return [Some _] if [sub] is a
    substring of [candidate], [None] otherwise.. *)

val match_prefix : ?case:bool -> candidate:string -> string -> result option

val fuzzy_match : ?case:bool -> candidate:string -> string -> result option
(** [fuzzy_match ?case ~candidate pattern] tries to find the letters of
    [pattern] in [candidate] (in the right order).

    For example:
       - [pattern = "tuou"] and [candidate = "tremulous"] will match.
       - [pattern = "touu"] and [candidate = "tremulous"] won't. *)

val fuzzy_prefix : ?case:bool -> candidate:string -> string -> result option
(** Same as [fuzzy_match] except that the first letter in [pattern] must be the
    first letter of [candidate]. *)
