(** This module handles the selection of a specific entry using arrows
    as in dmenu. *)

type 'a t = {
  unvisible_left: 'a list;
  visible_left: 'a list;
  selected: 'a option;
  visible_right: 'a list;
  unvisible_right: 'a list;
}
(** Data for pagination, it is a double zipper. *)

val from_list : ('a list -> 'a list * 'a list) -> 'a list -> 'a t
val left: 'a t -> 'a t
val right: 'a t -> 'a t
val page_left: ('a list -> 'a list * 'a list) -> 'a t -> 'a t
val page_right: ('a list -> 'a list * 'a list) -> 'a meta -> 'a t
