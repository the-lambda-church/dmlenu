(** Completion programs *)


type t = {
  sources: Source.t list; (** The sources we currently complete against *)
  transition : Candidate.t -> t; (** The transition function. *)
}
(** A completion program is a way to provide different completion for
    different tokens. [sources] are used to generate the possible
    tokens and when the user has picked one, it uses [transition] to
    know how to generate the possibilities for the next token, and so on. *)

val empty : t
(** The empty program *)

val singleton : Source.t -> t
(** Makes a program that completes only one token using its argument *)

val iterate : Source.t list -> t
(** [iterate sources] offers completions from sources, indefinitely. *)

val concat : t -> t -> t
(** Concatenates two machines. The first one is considered done when it returns
    []. *)

val csum : (string * t Lazy.t) list -> t
(** [csum l] offers the completion over [List.map fst l] and then
    proceeds as the chosen program. *)
  
