let rec large_firsts z =
match z with
| [] -> []
| (h1,h2) :: t -> 
  if h1 > h2 then
    h1 :: large_firsts t
  else
    large_firsts t;;

let large_firstsG z = 
  List.map (fun (h1,h2) -> h1) (
    List.filter (fun (h1,h2) -> h1 > h2) z
  );;
(* use List.map and List.filter, but do NOT use 'match' on a list *)
let rec mult_pos z = 
match z with
| [] -> 1
| (h :: t) -> 
  if h > 0 then 
    h * mult_pos t
  else
    1 * mult_pos t;;

let mult_posG z =
List.fold_right (fun list_item accumulator -> if list_item > 0 then list_item * accumulator else accumulator) z 1;;

let prep_avg z =
  let use_tuple list_item (count, total) =
    (count + 1, total + list_item)
  in
  List.fold_right use_tuple z (0, 0);;

(* List.fold_right (fun list_item accumulator_tuple -> 
  let count, sum = accumulator_tuple
  accumulator_tuple = (count + 1, sum + list_item) ) z (0,0);; *)
(* use List.fold_right, but NOT List.map/filter, or 'match' on a list *)