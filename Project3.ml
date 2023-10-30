(* === Parser and Interpreter for the language given in Project 3, CIS505/705, 
       Kansas State University, Fall 2023

This is a skeleton, with 7 places where code needs to be changed.
Each such line is marked with "CHANGE #k" where k is a number,
indicating the suggested order in which to make the changes.

Even the current skeleton enables the user to have dialogues such as

# run "+ 2 3" ;;
- : value = NumV 5

The 7 tasks are

#1 === this is about how to evaluate an identifier;
     it should be looked up in the environment

#2 === this is about how to evaluate a function definition;
      the appropriate closure should be created

#3 === this is about how to evaluate a function application,
    after the function part has been evaluated to a closure
      and after the argument part has been evaluated;
    this involves calling the closure body in an environment
       that implements static scope 

The above 3 changes will allow us to run all programs that involve only function application and abstraction, and for example get the dialogue

# run "app lam x + x 7   
         2"  ;;
- : value = NumV 9

#4 === how to parse 'let'
    take inspiration from how 'lam' is parsed.

This will allow the dialogue

# run "let x + 1 2 + x 4" ;;
- : value = NumV 7

and even the factorial program from the question text:

# run "
   let Z lam f app lam x app f lam v app app x x v 
                   lam x app f lam v app app x x v
   let Fact lam f lam n 
      if n 
         * n app f - n 1 
         1
  app app Z Fact 5" ;;
- : value = NumV 120

#5 === this is about how to handle the tuple operator which creates a
        table with a single entry

This will allow the dialogue

# run "tuple a b" ;;
- : value = TableV [("a", "b")]

#6 === this is about how to implement '-' when applied to tables

This will allow the dialogue

# run "- + tuple a b tuple c d tuple a b" ;;
- : value = TableV [("c", "d")]

#7 === this is about how to implement '*' when applied to two tables;
          the outline of the code is already there but with several flaws

This will allow the dialogue

run "let table1 + + + tuple a1 b1 tuple a2 b2 tuple a3 b2 tuple a2 b3
     let table2 + + + tuple b3 c1 tuple b2 c3 tuple b2 c4 tuple b4 c2
     * table1 table2" ;;
- : value =
TableV [("a2", "c3"); ("a2", "c4"); ("a3", "c3"); ("a3", "c4"); ("a2", "c1")]

and allow you to handle also all other programs from the question text!

You should develop your interpreter incrementally, 
and make sure that each new change type checks! 
(If you do not type check until you have made all the required changes,
it is rather likely that you'll get a bunch of error messages 
that may be very hard to understand and fix.)

*)

(* CONCRETE SYNTAX

exp ::= id
     |  num
     |  "lam" id exp
     |  "app" exp1 exp2
     |  "let" id exp1 exp2
     |  "if" exp1 exp2 exp3
     |  "tuple" id1 id2
     |  "select1" id1 exp0
     |  "select2" id2 exp0
     |  op exp1 exp2 

 op ::= "+"  (overloaded)
     |  "-"  (overloaded)
     |  "*"  (overloaded)

*)

(* EXCEPTIONS *)

exception InputEndsTooEarly
exception InputEndsTooLate
exception IdentifierExpectedBut of string
exception NotDeclared of string
exception TestNotInteger
exception ApplyNotClosure
exception SelectNotTable
exception PlusWrongArgs
exception MinusWrongArgs
exception TimesWrongArgs
exception OutputClosure

(* ABSTRACT SYNTAX *)

type identifier = string

type expE =
 | IdE of identifier
 | NumE of int
 | FunE of identifier * expE
 | ApplyE of expE * expE
 | LetE of identifier * expE * expE
 | IfE of expE * expE * expE
 | TupleE of identifier * identifier
 | Select1E of identifier * expE
 | Select2E of identifier * expE
 | PlusE of expE * expE
 | MinusE of expE * expE
 | TimesE of expE * expE

(* SCANNER
    converts the input string into a list of "tokens" *)

type tokenT = 
 | FunT
 | ApplyT
 | LetT
 | IfT
 | TupleT
 | Select1T
 | Select2T
 | PlusT
 | MinusT
 | TimesT
 | IdT of identifier
 | NumT of int

let print_token token = match token with
 | FunT -> "lam"
 | ApplyT -> "app"
 | LetT -> "let"
 | IfT -> "if"
 | TupleT -> "tuple"
 | Select1T -> "select1"
 | Select2T -> "select2"
 | PlusT -> "+"
 | MinusT -> "-"
 | TimesT -> "*"
 | (IdT id) -> ("identifier "^id)
 | (NumT n) -> "number"

let is_digit(ch) = 
   Char.code ch >= Char.code '0' && Char.code ch <= Char.code '9'

let char2digit(ch) = Char.code ch - Char.code '0'

let is_letter(ch) = 
    (Char.code ch >= Char.code 'a' && Char.code ch <= Char.code 'z')
 || (Char.code ch >= Char.code 'A' && Char.code ch <= Char.code 'Z')

let scanNum : string -> (int * string) = fun str ->
  let rec get_num acc str = 
    if str = "" 
    then (acc, str)
    else 
      let c = String.get str 0 and 
          str' = String.sub str 1 (String.length str - 1) in
      if is_digit c
      then get_num (10 * acc + (char2digit c)) str' 
      else (acc, str)
 in get_num 0 str

let scanId : string -> (string * string) = fun str ->
  let rec get_id acc str = 
    if str = "" 
    then (acc, str)
    else 
      let c = String.get str 0 and 
          str' = String.sub str 1 (String.length str - 1) in
      if is_letter c || is_digit c || c = '_'
      then get_id (acc ^ (String.make 1 c)) str'
      else (acc, str)
 in get_id "" str

let rec scan : string -> tokenT list = 
  fun str -> 
   if str = ""
   then []
   else let c = String.get str 0 
        and str1 = String.sub str 1 (String.length str - 1) in
   if is_digit c
   then let (n,str') = scanNum str
         in (NumT n :: (scan str'))
   else if is_letter (c)
   then let (s,str') = scanId str
     in let token =
       if s = "lam" then FunT
       else if s = "app" then ApplyT
       else if s = "let" then LetT
       else if s = "if" then IfT
       else if s = "tuple" then TupleT
       else if s = "select1" then Select1T
       else if s = "select2" then Select2T
       else IdT s
     in (token :: scan str')
   else match c with
     | '+' -> PlusT :: (scan str1)
     | '-' -> MinusT :: (scan str1)
     | '*' -> TimesT :: (scan str1)
     | _ -> scan str1

(* PARSER *)

let getIdT : tokenT list -> string * tokenT list =
  fun tokens -> 
   match tokens with
   | [] -> raise InputEndsTooEarly
   | (IdT id) :: tokens' -> (id, tokens')
   | (token :: _) -> 
       raise (IdentifierExpectedBut (print_token token))

let rec parseExp : tokenT list -> expE * tokenT list =
   fun tokens ->
    match tokens with
    | [] -> raise InputEndsTooEarly
    | (IdT s) :: tokens1 ->
         (IdE s, tokens1)
    | (NumT z) :: tokens1 ->
         (NumE z, tokens1)
    | ApplyT :: tokens1 ->
        let (e1, tokens2) = parseExp tokens1 in
        let (e2, tokens3) = parseExp tokens2 in
       (ApplyE(e1,e2), tokens3)
    | IfT :: tokens1 ->
        let (e1, tokens2) = parseExp tokens1 in
        let (e2, tokens3) = parseExp tokens2 in
        let (e3, tokens4) = parseExp tokens3 in
       (IfE(e1,e2,e3), tokens4)
    | TupleT :: tokens1 ->
         let (id1, tokens2) = getIdT tokens1  in
         let (id2, tokens3) = getIdT tokens2  in
       (TupleE (id1,id2), tokens3)
    | Select1T :: tokens1 ->
         let (id1, tokens2) = getIdT tokens1  in
         let (e1, tokens3) = parseExp tokens2 in
       (Select1E(id1,e1), tokens3)
    | Select2T :: tokens1 ->
         let (id1, tokens2) = getIdT tokens1  in
         let (e1, tokens3) = parseExp tokens2 in
       (Select2E(id1,e1), tokens3)
    | PlusT :: tokens1 ->
        let (e1, tokens2) = parseExp tokens1 in
        let (e2, tokens3) = parseExp tokens2 in
       (PlusE(e1,e2), tokens3)
    | MinusT :: tokens1 ->
        let (e1, tokens2) = parseExp tokens1 in
        let (e2, tokens3) = parseExp tokens2 in
       (MinusE(e1,e2), tokens3)
    | TimesT :: tokens1 ->
         let (e1, tokens2) = parseExp tokens1 in
         let (e2, tokens3) = parseExp tokens2 in
       (TimesE(e1,e2), tokens3)
    | FunT :: tokens1 ->
         let (fp, tokens2) = getIdT tokens1   in
         let (e0, tokens3) = parseExp tokens2 in
       (FunE(fp,e0), tokens3)
    | LetT :: tokens1 -> (NumE 47, tokens1) (* CHANGE #4 *)

let parse : string -> expE =
  fun input_string ->
    let tokens = scan input_string in
    let (exp,tokens1) = parseExp tokens
    in if tokens1 = []
       then exp
       else raise InputEndsTooLate

(* ENVIRONMENTS *)

type 'a environment =  identifier -> 'a

let initEnv : 'a environment = 
  fun id -> raise (NotDeclared id)

let insertEnv : identifier -> 'a -> 'a environment -> 'a environment =
  fun new_id a env ->
    fun id -> if id = new_id then a else env id

let retrieveEnv : 'a environment -> identifier -> 'a =
  fun env id -> env id

(* VALUES *)

type value =
   NumV of int
 | TableV of (identifier * identifier) list
 | ClosureV of identifier * expE * value environment

(* EVALUATING EXPRESSIONS *)

let rec eval exp env =
   match exp with
   | IdE id -> NumV 37 (* CHANGE #1 *) 
   | NumE n -> NumV n
   | FunE(id,exp1) -> NumV 57 (* CHANGE #2 *)
   | ApplyE(exp1,exp2) ->
       (match (eval exp1 env, eval exp2 env) with
         | (ClosureV(x,exp0,env0), v2) -> NumV 67 (* CHANGE #3 *)
         | _ -> raise ApplyNotClosure)
   | LetE(id,exp1,exp2) ->
       eval (ApplyE (FunE(id,exp2), exp1)) env
   | IfE(exp0,exp1,exp2) ->
       (match (eval exp0 env) with
         | NumV n ->
             if n > 0
             then eval exp1 env
             else eval exp2 env
         | _ -> raise TestNotInteger)
   | TupleE _ -> NumV 27    (* CHANGE #5 : make proper rule *)
   | Select1E(id,exp1) ->
       (match (eval exp1 env) with
         | (TableV tuples) ->
              TableV 
                (List.filter
                  (fun (id1,_) -> id1 = id)
                  tuples)
         | _ -> raise SelectNotTable)
   | Select2E(id,exp1) ->
       (match (eval exp1 env) with
         | (TableV tuples) ->
              TableV
                (List.filter
                  (fun (_,id2) -> id2 = id)
                  tuples)
         | _ -> raise SelectNotTable)
   | PlusE(exp1,exp2) ->
       (match (eval exp1 env, eval exp2 env) with
        | (NumV n1, NumV n2) -> NumV (n1 + n2)
        | (TableV tuples1, TableV tuples2) -> TableV (tuples1 @ tuples2)
        | _ -> raise PlusWrongArgs
       )
   | MinusE(exp1,exp2) ->
       (match (eval exp1 env, eval exp2 env) with
        | (NumV n1, NumV n2) -> NumV (n1 - n2)
              (* CHANGE #6 : allow subtraction on tables *)
        | _ -> raise MinusWrongArgs
       )
   | TimesE(exp1,exp2) ->
       (match (eval exp1 env, eval exp2 env) with
        | (NumV n1, NumV n2) -> NumV (n1 * n2)
        | (TableV tuples1, TableV tuples2) ->
            TableV  (* CHANGE #7 (the lines below)  *) 
              (List.map
                 (fun (a,b,c,d) -> (b,d))
                 (List.filter
                   (fun (a,b,c,d) -> a = c)
                   (List.fold_right 
                      List.append
                      (List.map 
                         (fun (a,b) ->
                            List.map 
                              (fun (c,d) -> (c,d,a,b))
                              tuples2)
                         tuples1)
                      [] )))
        | _ -> raise TimesWrongArgs
       )

let run x = 
  try
    (match (eval (parse x) initEnv) with
       | NumV n -> NumV n
       | TableV tuples -> TableV tuples
       | ClosureV _ -> raise OutputClosure
    ) with
   | InputEndsTooEarly -> 
       (print_string "input prematurely exhausted\n"; NumV 0)
   | InputEndsTooLate ->
       (print_string "input continues after expression is parsed\n"; NumV 0)
   | IdentifierExpectedBut s ->
       (print_string ("identifier expected but "^s^" seen\n"); NumV 0)
   | NotDeclared s ->
       (print_string ("identifier "^s^" not bound to value\n"); NumV 0)
   | TestNotInteger ->
       (print_string "test expression not integer\n"; NumV 0)
   | ApplyNotClosure ->
       (print_string "function part of application does not evaluate to closure\n"; NumV 0)
   | SelectNotTable ->
       (print_string "2nd argument to selection does not evaluate to table\n"; NumV 0)
   | PlusWrongArgs ->
       (print_string "'+' is not given two integers or two tables\n"; NumV 0)
   | MinusWrongArgs ->
       (print_string "'-' is not given two integers or two tables\n"; NumV 0)
   | TimesWrongArgs ->
       (print_string "'*' is not given two integers or two tables\n"; NumV 0)
   | OutputClosure ->
       (print_string "program returns closure\n"; NumV 0)
