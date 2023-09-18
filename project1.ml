(* INTERPRETER FOR A SMALL INTERACTIVE HIGHER-ORDER CALCULATOR 
      CIS505/705, K-State, Fall 2023 *)

(*

This is a skeleton, with 10 lines where code needs to be changed/inserted.
Each such line is marked with "CHANGE #k" where k is a number,
indicating the suggested order in which to make the changes.

Even the current skeleton enables the user to
  + display the top of the stack
  + display the full stack
  + exit the calculator

The 10 tasks are

 #1 === Enter a number

this will allow the dialogue:

 ? 29
 ? 16
 ? D
Stack (top first): 16 , 29

  #2 === Make a copy of the top of the stack

this will allow the dialogue:
  
? 28 
? 40
? C
? D
Stack (top first): 40, 40, 28

  #3 === Apply the current function to the top of the stack
      replacing it with the result

this will allow (since addition is implemented) the dialogue 

? 18
? 7
? +
? A
? D
Stack (top first): 25

  #4 === Create a function that multiplies a number

this will allow the dialogue:

? 8
? *
? 7
? A
? D
Stack (top first): 56

  #5 === Create a function that subtracts a number

this will allow the dialogue:

? 15
? -
? 23
? A
? D
Stack (top first): 8

  #6 === Replace the current function with the result of applying it twice

this will allow the dialogue:

? 4
? +
? W
? 5
? A
? D
Stack (top first): 13

  #7 === Update the dictionary 
  #8 === Retrieve a function from the dictionary

these will allow the dialogue:

? 7
? *
? S
which name to give the current function?
times7
? 4
? +
? S
which name to give the current function?
add4
? 2
? +
? 3
? A
? D
Stack (top first): 5
? R
which function must be retrieved?
times7
? 9
? A
? D
Stack (top first): 63 , 5

  #9  ===
     Replace the top of the function stack by the result of iterating it
       as many times as indicated by the top of the number stack
        (if that number is negative, an error should be reported!)
      The function 'repeat', cf. Lab 1, has been provided.

this will allow the dialogue:

? 2
? +
? 3
? I
? 5
? A
? D
Stack (top first): 11

  #10  === Let the current function take input that is prepped
            by a named function, as described in the question text.

this will allow the last dialogue mentioned in the question text.

You should develop your interpreter incrementally, 
and make sure that each new change type checks! 
(If you do not type check until you have made all the required changes,
it is rather likely that you'll get a bunch of error messages 
that may be hard to understand and fix.)

*)

(* EXCEPTIONS FOR ERRORS *)

exception IllformedInput
exception StackEmpty
exception KeyNotFound
exception RepeatNegative

(* EXCEPTIONS FOR SIDE EFFECTS *)

exception Exit
exception TopStack
exception DisplayStack

(* AUXILIARY FUNCTIONS *)

let rec repeat f x n =  
   if n = 0
   then x
   else repeat f (f x) (n-1)

(* DICTIONARY OPERATIONS *)

let initDict = []

let updateDict new_key new_val dictionary =
   (new_key, new_val) :: dictionary

let rec lookupDict dictionary key =
   match dictionary with 
    | [] -> raise KeyNotFound (* CHANGE #8 DONE*)
    | ((key1,val1) :: dictionary') ->
      if key = key1
      then val1
      else lookupDict dictionary' key

(* DECOMPOSING THE STACK *)

let top_of stack =
   match stack with
    | [] -> raise StackEmpty
    | n :: stack' -> n

let rest_of stack =
   match stack with
    | [] -> raise StackEmpty
    | n :: stack' -> stack'

(* INTERPRETING THE INPUT *)

let get_number str = 
  let rec get_num str acc = 
    if str = "" 
    then acc
    else 
      let c = String.get str 0 and 
          str' = String.sub str 1 (String.length str - 1) in
      let d =  Char.code c  - Char.code '0' in
      if d >= 0 && d < 10 
      then 
        let m =
          match acc with
           | None -> d
           | Some n -> 10 * n + d
        in get_num str' (Some m)
      else acc
  in get_num str None


(* THE READ-EVAL-PRINT LOOP *)

(* interpret: 
      int list                        Stack of Numbers
   -> (string * (int -> int)) list    Dictionary of Functions
   -> (int -> int)                    Current Function 
   ->  unit                     
 *)

let rec interpret stack dict curfun =  
     (* READ, AND THEN EVAL BASED ON WHAT IS READ *)
  (print_string "? " ;
   let inp = read_line () in
   try 
    (match inp with
     | "A" -> 
          interpret ((curfun (top_of stack)) :: rest_of stack) dict curfun (* CHANGE #3 DONE*)
     | "C" -> 
          let z = top_of stack in
          interpret (z :: stack) dict curfun (* CHANGE #2 DONE*)
     | "D" -> raise DisplayStack
     | "I" -> 
          let n = top_of stack in
          let repeat_x_times x = repeat curfun x n in
            if n < 0
            then raise RepeatNegative
            else interpret (rest_of stack) dict (repeat_x_times) (* CHANGE #9 *)
     | "P" ->  
          (print_string "which function should prep for the current function?\n";
            let str = read_line () in
            let prep = lookupDict dict str in
            let newfun x = curfun (prep x) in
            interpret stack dict newfun (* CHANGE #10 *) 
          )
     | "R" -> 
          (print_string "which function must be retrieved?\n";
            let str = read_line () in
            interpret stack dict (lookupDict dict str) 
          )
     | "S" -> 
          (print_string "which name to give the current function?\n";
            let str = read_line () in
            interpret stack (updateDict str curfun dict) curfun (* CHANGE #7 DONE*) 
          )
     | "T" -> raise TopStack 
     | "W" -> 
          let new_fun x = curfun (curfun x) in
          interpret stack dict (new_fun) (* CHANGE #6 *)
     | "X" -> raise Exit
     | "+" -> 
          interpret (rest_of stack) dict ((+) (top_of stack))
     | "*" -> 
          interpret (rest_of stack) dict (( * ) (top_of stack)) (* CHANGE #4  DONE*)
     | "-" ->  
          interpret (rest_of stack) dict ((+) (-(top_of stack))) (* CHANGE #5 DONE*)
     | _ -> 
        (match get_number inp with
         | Some n -> interpret (n::stack) dict curfun (* CHANGE #1  DONE*)
         | None -> raise IllformedInput)
    ) with Exit ->
         (print_string "Hopefully you enjoyed using the CIS505/705 calculator! Bye\n"; 
            ())
    |   excp ->
          (print_string
            (match excp with
              | TopStack ->
                  (match stack with
                    | [] -> "Stack is empty\n"
                    | n :: _ -> "Top of stack: "^(Int.to_string n)^"\n")
              | DisplayStack -> 
                  "Stack (top first): "^
                  (String.concat " , " (List.map Int.to_string stack))^"\n"
              | IllformedInput -> "Input ill-formed\n"
              | StackEmpty -> "Stack is empty\n"
              | KeyNotFound -> "Identifier not found in dictionary\n"
              | RepeatNegative -> "Iterate called on negative number\n"
              | _ -> " *** redundant ***\n");
            interpret stack dict curfun)
  )

(* INVOCATION *)
 (* run: unit -> unit *)

let run () = 
  (print_string "The CIS505/705 Calculator is ready!\n";
    interpret        (* initially,                   *)
      []             (* number stack is empty        *)
      initDict       (* dictionary is empty          *)
      (fun x -> x)   (* current function is identity *)
   )

