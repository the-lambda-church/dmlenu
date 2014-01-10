type 'a t = {
  unvisible_left: 'a list;
  visible_left: 'a list;
  selected: 'a option;
  visible_right: 'a list;
  unvisible_right: 'a list;
}


let empty = {
  unvisible_left = []; visible_left = [];
  selected = None; visible_right = []; 
  unvisible_right = []
}
let from_list split = function
  | [] -> empty
  | t :: q -> 
    let a, b = split (t :: q) in
    { empty with 
      selected = Some t;
      visible_right = List.tl a;
      unvisible_right = b
    }


let left p =
