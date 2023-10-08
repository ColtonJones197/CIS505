type 'a bin_tree = 
    Leaf of 'a
  | Node of 'a bin_tree    (* left tree *)
          * 'a bin_tree;;    (* right tree *)

let tree1 =
  Node 
   (Node
     (Node (Leaf 7, Leaf 0),
      Leaf 4),
    Node
     (Leaf 8,
      Node (Leaf 2, Leaf 6)));;

let tree2 =
  Node 
   (Node
     (Node (Leaf 4, Leaf 0),
      Leaf 7),
    Node
     (Leaf 8,
      Node (Leaf 9, Leaf 6)));;

let rec height t = 
   match t with 
  | Leaf _ -> 0
  | Node (left,right) ->
      1 + max (height left) (height right);;

let rec sum t =
   match t with
   | Leaf n -> n
   | Node (left,right) ->
       (sum left) + (sum right);;

(* TASK 1  *)
let rec contorted t =
  match t with
  | Leaf x -> false
  | Node (left,right) ->
      if (height (left) > height (right) && sum (left) < sum (right))
        || (height(left) < height(right) && sum(left) > sum(right)) then true
      else contorted left || contorted right;;


let rec foldt f e t =
   match t with
  | Leaf x -> e x
  | Node (left,right) ->
      f (foldt f e left) (foldt f e right);;

let height2 t =
  foldt 
      (fun h1 h2 -> 1 + max h1 h2) 
      (fun n -> 0) 
      t;;

(* TASK 2

let height2 t =
  foldt 
     (fun h1 h2 -> ...) 
     (fun n -> ...) 
     t

let sum2 t =
  foldt
     ...
     ...
     ...

let contorted2 t =
  foldt 
    (fun (h1,s1,c1) (h2,s2,c2) ->
       ...         )
    (fun n -> ...)
    t

*)

let rec mk_tree n = 
   if n = 0 then Leaf 0
   else let tree' = mk_tree (n-1)
    in Node
        (Node
          (Leaf n, tree'),
         Node
          (Leaf 11, 
            Node (tree', Leaf 11)));;
