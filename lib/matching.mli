(** Functions used to match candidates. *)

(** Default sources (defined in {!Sources}) use the default
    {!match_query} match function that you can change using
    {!set_match_query_fun} (in hooks for instance).
*)
type result = ((bool * int * int) list)
(** The return type of matching functions.  If a matching function succeeded
    returning [Some l], then [l] must be a list of terms [(match, start, stop)],
    splitting the candidate into substrings, where [match] describes whether the
    substring is matched or not. There must be at least one substring with
    [match] being [true].

    For example: if substring ["bar"] of candidate ["foobarbaz"] is matched, the
    corresponding {!result} is [[(false, 0, 3); (true, 3, 6); (false, 6, 9)]].
*)

type t = string -> result option
(** The type of matching functions *)

(** {3 Matching function} *)

val match_query : candidate:string -> t
(** The default matching function *)

val set_match_query_fun : (candidate:string -> t) -> unit
(** Set the default matching function *)

(** {2 Predefined matching functions} *)

(** NB: for the following functions the default value of [case] is [true] (case sensitive search). *)

val subset : ?case:bool -> candidate:string -> t
(** [subset ?case ~candidate query] will match if the words of the query
    can be found in the candidate (in any order, and possibly overlapping).
*)

val partial_match : ?case:bool -> candidate:string -> t
(** [partial_match ?case ~candidate sub] will return [Some _] if [sub] is a
    substring of [candidate], [None] otherwise.. *)

val match_prefix : ?case:bool -> candidate:string -> t

val fuzzy_match : ?case:bool -> candidate:string -> t
(** [fuzzy_match ?case ~candidate pattern] tries to find the letters of
    [pattern] in [candidate] (in the right order).

    For example:
       - [pattern = "tuou"] and [candidate = "tremulous"] will match.
       - [pattern = "touu"] and [candidate = "tremulous"] won't. *)

val fuzzy_prefix : ?case:bool -> candidate:string -> t
(** Same as [fuzzy_match] except that the first letter in [pattern] must be the
    first letter of [candidate]. *)

val trivial : candidate: string -> t
(** [trivial ~candidate] always matches its input. Used when you want to
    some two-dimensional magic. *)

val on : (string -> string) -> t -> t
(** [on f matching_function] returns a new matching function that applies
    [matching_function] on the output of [f] *)
