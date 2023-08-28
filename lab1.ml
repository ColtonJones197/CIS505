let twice f x = f (f x);;
(* twice (fun x -> x + 3) 2;; *)

let rec repeat f (x: int) (n: int) =
  (* if n < 0 then -1 else *)
  match n with
  | 0 -> x
  | 1 -> f x
  | _ -> f (repeat f x (n - 1));;
(* repeat (fun x -> x + 7) 4 3;; *)

let rec times (x: int) (n: int) =
  (+) (repeat times x (n-1)) x;;

times 11 7;;