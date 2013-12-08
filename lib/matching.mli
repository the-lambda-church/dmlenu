type result = ((bool * int * int) list)
(** The type of return of matching functions.  If it succeeded Some l,
    a list of terms (match, start, stop) that denotes substring of the
    candidates that are matched or not. *)

val match_query : case: bool -> query: string -> candidate: string -> result option
(** [match_query ~case ~query ~candidate] returns the result of
    matching [candidate] against [query]. If [case] is true then the
    matching is case sensitive. *)
