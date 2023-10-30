(* === various test sets for Project 3, 2023 *)

(* -- Basic Features *)

let test1 = run "app lam x x 6"
 (*
val test1 : value = NumV 6
 *)

let test2 = run "app lam x + x 7 3"
 (*
val test2 : value = NumV 10
 *)

let test3 = run 
 "let plus5 lam x + x 5
    app plus5 28"
 (*
val test3 : value = NumV 33
 *)
 
let test4 = run
  "let plus lam x lam y + x y 
   let plusseven app plus 7
  app plusseven 6"
 (*
val test4 : value = NumV 13
 *)

let test5 = run
  "let twice lam H lam x app H app H x
   let multfour lam x * 4 x 
  app app twice multfour 5"
 (*
val test5 : value = NumV 80
 *)

let testFact0 = run
  "let fact lam n
    if n 
     * n 
       app fact - n 1 
     1 
  app fact 6"
 (*
identifier fact not bound to value
val testFact0 : value = NumV 0
 *)

(* -- Major Examples *)

let testFact = run
  "let Z lam f app lam x app f lam v app app x x v lam x app f lam v app app x x v
   let Fact lam f lam n 
   if n 
      * n app f - n 1 
      1
  app app Z Fact 5"
 (*
val testFact : value = NumV 120
 *)


(* -- Table Operators *)

let test6 = run 
  "tuple France Paris"
(*
val test6 : value = TableV [("France", "Paris")]
*)

let test7 = run
  "+ tuple France Paris 
     tuple Spain Madrid"
(*
val test7 : value = TableV [("France", "Paris"); ("Spain", "Madrid")]
*)

let test8 = run
  "
let two + tuple France Paris 
          tuple Spain Madrid
let one tuple France Paris
- two one"

(*
val test8 : value = TableV [("Spain", "Madrid")]
*)

let test9 = run
 "let table + + tuple France Paris tuple Spain Madrid tuple France Nice
   select1 France table"
(*
val test9 : value = TableV [("France", "Paris"); ("France", "Nice")]
*)

let test10 = run "
  let table + + tuple France Paris tuple Italy Rome tuple Finland Helsinki
     select2 Rome table"
(*
val test10 : value = TableV [("Italy", "Rome")]
*)

let test11 = run "
  let table1 + + + tuple a1 b1 tuple a2 b2 tuple a3 b2 tuple a2 b3
  let table2 + + + tuple b3 c1 tuple b2 c3 tuple b2 c4 tuple b4 c2
* table1 table2"
(*
val test11 : value =
  TableV
   [("a2", "c3"); ("a2", "c4"); ("a3", "c3"); ("a3", "c4"); ("a2", "c1")]
*)

let test12 = run "
 let countries 
      +++ tuple Asia Japan 
          tuple Asia China
        + tuple Europe UK 
          tuple America Canada
       ++ tuple America US
          tuple Europe Italy
        + tuple Africa Kenya
          tuple Europe France
 let capitals
      +++ tuple Japan Tokyo 
          tuple France Paris
        + tuple Italy Rome 
          tuple UK London
       ++ tuple Canada Ottawa 
          tuple US DC
        + tuple Kenya Nairobi
          tuple China Beijing
 let join * countries capitals
  - + + select1 Asia join select1 America join select1 Africa join 
    tuple America Ottawa"
(*
val test12 : value =
  TableV
   [("Asia", "Tokyo"); ("Asia", "Beijing"); ("America", "DC");
    ("Africa", "Nairobi")]
*)

(* -- Typical Semantic Errors *)

let errorSemantic1 = run "app lam x w 7"
  (*
identifier w not bound to value
val errorSemantic1 : value = NumV 0
  *)

let errorSemantic2 = run "lam z + z 4"
  (*
program returns closure
val errorSemantic2 : value = NumV 0
  *)

let errorSemantic3 = run "app 8 7"
  (*
function part of application does not evaluate to closure
val errorSemantic3 : value = NumV 0
  *)

let errorSemantic4 = run "if lam x x 6 7"
  (*
test expression not integer
val errorSemantic4 : value = NumV 0
  *)

let errorSemantic5 = run "+ 4 lam x x"
  (*
'+' is not given two integers or two tables
val errorSemantic5 : value = NumV 0
  *)

let errorSemantic6 = run "- 7 lam x x"
  (*
'-' is not given two integers or two tables
val errorSemantic6 : value = NumV 0
  *)

let errorSemantic7 = run "* lam x x 7"
  (*
'*' is not given two integers or two tables
val errorSemantic7 : value = NumV 0
  *)

let errorSemantic8 = run "select1 P 7"
 (*
2nd argument to selection does not evaluate to table
val errorSemantic8 : value = NumV 0
*)

(* -- Typical Syntax Errors *)

let errorSyntax1 = run "+ 3"
 (*
input prematurely exhausted
val errorSyntax1 : value = NumV 0
 *)

let errorSyntax2 = run "+ 3 4 5"
 (*
input continues after expression is parsed
val errorSyntax2 : value = NumV 0
 *)

let errorSyntax3 = run "let + 4 5"
 (*
identifier expected but + seen
val errorSyntax3 : value = NumV 0
 *)
