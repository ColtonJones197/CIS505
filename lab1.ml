let twice f x = f (f x);;
(* twice (fun x -> x + 3) 2;; *)

let rec repeat f (x: int) (n: int) =
  match n with
  | -1 -> 0
  | 0 -> x
  | 1 -> f x
  | _ -> f (repeat f x (n - 1));;
(* repeat (fun x -> x + 7) 4 3;; *)

let times (x: int) (n: int) =
  repeat ((+) x) x (n-1);;

(* times 11 7;; *)

let power base pow =
  repeat (times base) base (pow - 1);;

(* power 5 4;; *)