(** Completion programs *)

type t = {
  sources: Source.t list; (** The sources we currently complete against *)
  transition : Candidate.t -> t; (** The transition function. *)
  completion: Source.t list; 
   (** The sources used to actually complete the display fields of the sources.
       It is only when the lines mode is set. *)
       
}

val empty : t
(** The empty program *)

val singleton : ?completion : Source.t list -> Source.t -> t
(** Makes a program that completes only one token using its argument *)

val iterate : ?completion : Source.t list -> Source.t list -> t
(** [iterate sources] offers completions from sources, indefinitely. *)

val concat : t -> t -> t
(** Concatenates two machines. The first one is considered done when it returns
    []. *)

val csum : (string * t Lazy.t) list -> t
(** [csum l] offers the completion over [List.map fst l] and then
    proceeds as the chosen program. *)
  
