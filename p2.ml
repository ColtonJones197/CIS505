(* INTERPRETER FOR A SMALL IMPERATIVE LANGUAGE
      CIS505/705, K-State, Fall 2023 

 The 'interpret' function assumes a 'parse' function, 
      written in another file.

This is a skeleton, with 10 lines where code needs to be changed/inserted.
Each such line is marked with "CHANGE #k" where k is a number,
indicating the suggested order in which to make the changes.

Even the current skeleton enables the user to have dialogues such as

# interpret "write 7" [] ;;
- : outputStream = [7]

The 10 tasks are

#1 === how to execute a sequence of commands
  Make sure it is in the right order, and that each command has an effect!
  
This will allow the dialogue

# interpret "write 4; write 5" [] ;;
- : outputStream = [4; 5]

#2 === how to execute an assignment
  Remember that the value of the right hand side should get 
    into the location denoted by the left hand side.

This will allow the dialogue

# interpret "variable x; x = 6; write x" [] ;;
- : outputStream = [6]

and also the dialogue

# interpret "array B[4]; [B 1] = 7; [B 2] = 9; write [B 1]" [] ;;
- : outputStream = [7]

#3 ===  how to read from an empty input stream
  As stated in the question text, you must raise a suitable exception

This will allow the dialogue

# interpret "variable w; read w" [] ;;
*** ERROR: input stream prematurely exhausted
- : outputStream = [0]

#4 === how to read from a non-empty input stream
  Take inspiration from #2 on how locEval computes a location
  
This will allow the dialogue

# interpret "variable x; variable y; read x; read y; write (x-y)" [8 ; 5] ;;
- : outputStream = [3]

and also the dialogue

# interpret "array A[5]; read [A 2]; read [A 3]; write ([A 3] - [A 2])" [1 ; 3] ;;
- : outputStream = [2]
 
#5 === how to execute a conditional
  Remember that positive integers are consider 'true'; 
    zero and negative integers are considered false.
  
This will allow the dialogue

# interpret "if (7 - 4) {if (4 - 7) {write 10} {write 11}} {write 12}" [] ;;
- : outputStream = [11]

#6 === how to execute a "while" loop
  Hint: the question text mentions an equivalent command.
  
This will allow the dialogue

# interpret "variable n; variable fac; read n; fac = 1; while n {fac = (fac * n); n = (n - 1)}; write fac" [6] ;;
- : outputStream = [720]

#7 === how to expand macros
  Hint: consult the appropriate environment.

This will allow the dialogue

# interpret "variable x; macro inc {x = (x + 1)}; read x; expand inc; expand inc; write x" [4] ;;
- : outputStream = [6]

 #8 === how to model aliasing
   Hint: update the appropriate environment

This will allow the dialogue

# interpret "variable x; variable y; alias ax x; alias ay y; read x; read ay; write y; write ax" [4 ; 9] ;;
- : outputStream = [9; 4]

and also the dialogue

# interpret "array A[5]; alias x [A 2] ; read [A 1] ; read [A 2] ; read [A 3] ; write x" [4 ; 5 ; 6] ;;
- : outputStream = [5]

 #9 === how to report that array index is out of bound

This will allow the dialogue

# interpret "array B[6]; [B 6] = 4" [] ;;
*** ERROR: index 6 but allowed range is 0..5
- : outputStrem = [0]

 #10 ===  how to properly handle array indices

This is the most challenging exercise;
the current code works for 1-dimensional arrays 
but you should augment it so as to handle 2-dimensional arrays,
and even (for extra credit) arrays of 3+ dimensions.

This will allow you to handle the matrix_multiplication program,
as well as all other programs from the question text!

You should develop your interpreter incrementally, 
and make sure that each new change type checks! 
(If you do not type check until you have made all the required changes,
it is rather likely that you'll get a bunch of error messages 
that may be very hard to understand and fix.)

*)

(* ABSTRACT SYNTAX (from parser)

type expE =
 | NumE of int
 | GetE of leftL
 | PlusE of expE * expE
 | MinusE of expE * expE
 | TimesE of expE * expE
and leftL =
 | LocationL of string
 | ReferenceL of leftL * expE 

type commC =
 | SkipC
 | SeqC of commC * commC
 | WriteC of expE
 | ReadC of leftL
 | IfC of expE * commC * commC
 | WhileC of expE * commC
 | ExpandC of string
 | MutateC of leftL * expE

type declD = 
 | DataD of string * int list
 | AliasD of string * leftL
 | MacroD of string * commC 

type progP = 
 | ProgP of declD list * commC
 | ErrorP of string   (* to report errors *)

*)

(* EXCEPTIONS *)

exception NotDeclared of string
exception InputExhausted  
exception IllegalBounds of string
exception TooManyIndices
exception TooFewIndices
exception IndexOutOfBounds of int * int

(* LOCATION RANGES *)

type location = int    

type bound = int

type location_range = location * bound list
  (* an integer variable x declared by 'variable x'
         may have location range (17, [])
     a variable A declared by 'array A[5]'
         may have location range (27, [5])
          such that say [A 2] is in location 29
     a variable B declared by 'array B[3,7]'
         may have location range (42, [3,7])
          such that say 
            [[B 0] 6] is in location 42 + 0 x 7 + 6 = 48
            [[B 1] 0] is in location 42 + 1 x 7 + 0 = 49
          observe that say
             [B 1]    has location range (49, [7])
             [B 2]    has location range (56, [7])
  *)

(* STORES *)

type value = int       

type store = location -> value

let initStore : store = 
  fun loc -> 0 (* all locations are initially zero *) 

let modifyStore :  location -> value -> store -> store =
  fun loc_mod v sto ->
    fun loc -> if loc = loc_mod then v else sto loc

let lookupStore : store -> location -> value =
  fun sto loc -> sto loc

(* ENVIRONMENTS *)

type variable = string

type 'a environment =  variable -> 'a

let initEnv : 'a environment = 
  fun id -> raise (NotDeclared id)

let insertEnv : variable -> 'a -> 'a environment -> 'a environment =
  fun new_id a env ->
    fun id -> if id = new_id then a else env id

let retrieveEnv : 'a environment -> variable -> 'a =
  fun env id -> env id

type dataEnv = location_range environment
  (* associates data variables with locations and with their bounds. *)

type macroEnv = commC environment
  (* associates macro names with commands *)

(* EVALUATION OF EXPRESSIONS *)

let rec expEval : expE -> dataEnv -> store -> value =
  fun exp denv sto ->
   match exp with
   | (NumE n) -> n
   | (GetE lhs) -> 
        let loc = locEval lhs denv sto
         in lookupStore sto loc 
   | PlusE (exp1,exp2) ->
        let v1 = expEval exp1 denv sto
        and v2 = expEval exp2 denv sto
       in v1 + v2
   | MinusE (exp1,exp2) ->
        let v1 = expEval exp1 denv sto
        and v2 = expEval exp2 denv sto
       in v1 - v2
   | TimesE (exp1,exp2) ->
        let v1 = expEval exp1 denv sto
        and v2 = expEval exp2 denv sto
       in v1 * v2

and leftEval : leftL -> dataEnv -> store -> location_range =
  fun left denv sto ->
   match left with
    | LocationL id -> retrieveEnv denv id
    | ReferenceL (left1, index_exp) ->
        let (loc, bounds) = leftEval left1 denv sto
        and index = expEval index_exp denv sto
       in match bounds with
          | [] -> raise TooManyIndices
          | bound1 :: bounds' ->
              if true (* CHANGE #9 *)
              then (loc + index, bounds') (* CHANGE #10 *)
              else raise (IndexOutOfBounds(bound1, index))

and locEval : leftL -> dataEnv -> store -> location =
  fun left denv sto ->
    let (loc, bounds) = leftEval left denv sto
   in match bounds with
      | [] -> loc
      | _ -> raise TooFewIndices

(* EXECUTION OF COMMANDS *)

type inputStream = int list
type outputStream = value list
type runTimeState = inputStream * outputStream * store

let rec commExec: 
     commC -> dataEnv -> macroEnv -> runTimeState -> runTimeState =
  fun cmd denv penv state ->
   match state with
    (inp,outp,sto) ->
   match cmd with
   | SkipC -> state
   | SeqC(cmd1,cmd2) ->
       let state1 = commExec cmd1 denv penv state in
       let state2 = commExec cmd2 denv penv state in state   (* CHANGE #1 *)
   | WriteC exp -> 
        let v = expEval exp denv sto
       in (inp, outp @ [v], sto)  
             (* if write is long we may want to do (v :: outp)
                  and then reverse at the end *) 
   | ReadC lhs ->
      (match inp with
       | [] -> ([], outp, sto)  (* CHANGE #3 *)
       | v :: inp' -> (inp, outp, sto))  (* CHANGE #4 *)
   | IfC(exp,cmd1,cmd2) ->
          commExec cmd1 denv penv state (* CHANGE #5 *)
   | WhileC(exp,cmd) ->
       commExec (IfC(exp,cmd,SkipC))  (* CHANGE #6 *)
           denv penv state
   | ExpandC id ->
       commExec SkipC     (* CHANGE #7 *)
           denv penv state   
   | MutateC(lhs,rhs) ->
       (inp, outp,
            sto)   (* CHANGE #2 *)

(* PROCESSING OF DECLARATIONs *)

type next = location (* next available location *)

let declExec: declD -> dataEnv * macroEnv * next
                    -> dataEnv * macroEnv * next =
  fun decl (denv,penv,next) -> 
   match decl with
   | DataD (id, bounds) ->
       if List.for_all (fun bound -> bound > 0) bounds
       then 
          (insertEnv id (next, bounds) denv,
           penv,
           next + (List.fold_right ( * ) bounds 1))
       else raise (IllegalBounds id)
   | MacroD (id, cmd) ->
       (denv, 
        insertEnv id cmd penv, 
        next)
   | AliasD(new_id, left) -> 
       let loc_range = leftEval left denv initStore
        in (denv,   (* CHANGE #8 *)
            penv, next)

let rec declsExec: 
   declD list -> dataEnv * macroEnv * next
              -> dataEnv * macroEnv * next =
  fun decls (denv,penv,next) -> 
   match decls with
   | [] -> (denv,penv,next)
   | decl1 :: decls' ->
       declsExec decls' (declExec decl1 (denv,penv,next))

(* RUNNING THE PROGRAM *)

let progRun : progP -> inputStream -> outputStream =
  fun prog inp ->
   match prog with
   | ProgP(decls,cmd) ->
      let (denv,menv,_) = declsExec decls (initEnv, initEnv, 0) in
      let (_, outp, _ ) = commExec cmd denv menv (inp, [], initStore)
     in outp
   | ErrorP s -> 
        (print_string ("*** Syntax Error: "^s^"\n"); [777])

let interpret : string -> inputStream -> outputStream =
  fun program_text input_stream ->
    try (progRun (parse program_text) input_stream)
    with
    | NotDeclared x ->
        (print_string 
           ("*** ERROR: "^x^" used but not declared\n"); 
         [0])
    | InputExhausted ->
        (print_string 
           ("*** ERROR: input stream prematurely exhausted\n"); 
         [0])
    | (IllegalBounds x) ->
        (print_string 
           ("*** ERROR: "^x^" declared with a non-positive bound\n"); 
         [0])
    | TooManyIndices ->
        (print_string
           ("*** ERROR: a scalar considered an array\n");
         [0])
    | TooFewIndices ->
        (print_string
           ("*** ERROR: an array considered a scalar\n");
         [0])
    | (IndexOutOfBounds(n,i)) ->
         (print_string 
           ("*** ERROR: index "^(Int.to_string i)^
            " but allowed range is 0.."^(Int.to_string (n-1))^"\n"); 
          [0])

