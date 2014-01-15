open Batteries

type 'a t = {
  unvisible_left: 'a list;
  visible: 'a list;
  selected: int;
  unvisible_right: 'a list;
}


let empty = {
  unvisible_left = []; visible = []; selected = 0;
  unvisible_right = []
}
let from_list split = function
  | [] -> empty
  | t :: q -> 
    let visible, unvisible = split (t :: q) in
    { empty with 
      selected = 0;
      visible; unvisible_right = unvisible;
    }


let page_left f p = 
  let visible, unvisible = f p.unvisible_left in
  match visible with
  | [] -> p
  | t :: q -> 
    { unvisible_left = unvisible; 
      unvisible_right = p.visible @ p.unvisible_right;
      visible = List.rev visible; selected = 0; }

let page_right f p = 
  let visible, unvisible = f p.unvisible_right in
  match visible with
  | [] -> p
  | t :: q -> 
    { unvisible_right = unvisible; 
      unvisible_left = List.rev p.visible @ p.unvisible_left;
      visible; selected = 0; }


let left f p = 
  if p.selected = 0 then
    let p' = page_left f p in
    { p' with selected = List.length p'.visible - 1 }
  else
    { p with selected = p.selected - 1 }

let right f p = 
  if p.selected = List.length p.visible - 1 then
    page_right f p
  else
    { p with selected = p.selected + 1 }

let fold_visible f state p = 
  List.fold_left (fun (counter, state) elem ->
    (counter + 1, f state (counter = p.selected) elem))
    (0, state)
    p.visible |> snd


let selected p = 
  if p.visible = [] then failwith "Pagination.selected: empty list"
  else List.nth p.visible p.selected

let is_empty p = p.visible = []
