(** This module handles the selection of a specific entry using arrows
    as in dmenu. *)

type 'a t = {
  unvisible_left: 'a list;
  visible: 'a list;
  selected: int;
  unvisible_right: 'a list;
  split: 'a list -> 'a list * 'a list;
}

(** Data for pagination, it is a double zipper. *)

val all : 'a t -> 'a list
(** Collects all the entry in a pagination data *)

val from_list : ('a list -> 'a list * 'a list) -> 'a list -> 'a t
(** Create a pagination data from a list where the first argument is
    used to determine how many events are visible (It takes a list of
    elements and return the initial segment of the list that would fit
    on the screen and the rest). The other events are placed on the
    right and the first element is selected *)

val left: 'a t -> 'a t
(** Move a pagination data to the left *)

val right: 'a t -> 'a t
(** Move a pagination data to the right *)
val page_left: 'a t -> 'a t
val page_right: 'a t -> 'a t
val fold_visible: ('b -> bool -> 'a -> 'b) -> 'b -> 'a t -> 'b
(** Fold over the visible elements of a pagination data.  The boolean
    tell you whether the current element is the selected one. *)
    
val is_empty: 'a t -> bool
val selected: 'a t -> 'a
