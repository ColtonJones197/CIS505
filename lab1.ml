let twice f x = f (f x);;
twice (fun x -> x + 3) 2;;