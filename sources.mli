(** Sources *)
open Completion

(** {3 Matching function & completion function} *)
val match_strict: string -> (string -> string -> match_result)
(** [match_strict s] is a matching
    function that matches only queries that
    are substrings of [s] *)

val match_in_word : string -> (string -> string -> match_result)
  -> (string -> string -> match_result)
(** [match_in_word separator f] matches a query if the current word
    (according to [separator]) matches f *)
val complete_in_word : ?drop_cont : bool -> string -> (string -> string -> string * string) -> string -> string -> string * string
(** [complete_in_word ?drop_cont separator f] completes only in the
    current word (according to separator). If drop_cont is true (default
    false), it will drop the words after the cursor. *)

type t = Completion.ex_source


val filename : t

val from_list : (string * string) list -> t

val kleene : string -> (t -> t)
val concat : string -> t -> t -> t
val binaries : t
val stdin : ?sep: string -> unit -> t
