  (* BASIC FEATURES *)
let test0 = "variable n; variable q; read q; read n; write (q-n); write (q+n)"
let result0 = interpret test0 [7; 4; 9]
  (* 
     val result0 : outputStream = [3; 11]
  *)

  (* WHILE LOOPS (factorial) *)
let test1 = "variable n; variable fac; read n; write n; fac = 1; while n {fac = (fac * n); n = (n-1)}; write fac"
let result1 = interpret test1 [6]
   (* 
      val result1 : outputStream = [6; 720]
   *)

  (* MACROS (factorial) *)
let test2 = "variable n; macro F {if n {fac = (fac * n); n = (n-1); expand F} {skip}}; variable fac; read n; write n; fac = 1; expand F; write fac"
let result2 = interpret test2 [5]
  (*
      val result2 : outputStream = [5; 120]
  *)

  (* renamed *)
let test2a = "variable n; macro fac {if n {fac = (fac * n); n = (n-1); expand fac} {skip}}; variable fac; read n; write n; fac = 1; expand fac; write fac"
let result2a = interpret test2a [7]
  (*
     val result2a : outputStream = [7; 5040]
  *)

  (* ONE-DIMENSIONAL ARRAYS *) 
let test3 = "array B[5]; variable q; read [B 1]; read [B 2]; q = [B 2]; [B 2] = [B 1]; [B 1] = q; [B 4] = ([B 1] + [B 2]); write [B 0]; write [B 1]; write [B 2]; write [B 3]; write [B 4]"
let result3 = interpret test3 [2; 6]
 (*
    val result3 : outputStream = [0; 6; 2; 0; 8]
 *)

  (* ALIASING INTEGER VARIABLES *)
let test4 = "variable x; variable y; alias ax x; variable z; alias aax ax; read x; read y; read z; write x; write y; write ax; write z; write aax"
let result4 = interpret test4 [5; 3; 8; 9]
  (*
val result4 : outputStream = [5; 3; 5; 8; 5]
  *)

  (* INSERTION SORT *) 
let test5 = "variable n; variable i; variable j; variable t;"^ 
   "array Q[20]; read n;"^
   "i = 0; while (n - i) {read [Q i]; i = (i + 1)};"^
   "i = 0; while (n - i)"^
     "{j = i;"^
     "while j {"^
        "if ([Q (j-1)] - [Q j])"^
          "{t = [Q (j-1)]; [Q (j-1)] = [Q j]; [Q j] = t; j = (j-1)}"^
          "{j = 0}};"^
     "i = (i+1)};"^
   "i = 0; while (n - i) {write [Q i]; i = (i+1)}"
let result5 = interpret test5 [7; 13; 11; 4; 8; 10; 17; 5]
 (* 
   val result5 : outputStream =
      [4; 5; 8; 10; 11; 13; 17]
 *)

  (* INSERTION SORT with ALIASING of second element *)
let test5a = "variable n; variable i; variable j; variable t;"^ 
   "array Q[20]; alias second [Q 1]; read n;"^
   "i = 0; while (n - i) {read [Q i]; i = (i + 1)};"^
   "i = 0; while (n - i)"^
     "{j = i;"^
     "while j {"^
        "if ([Q (j-1)] - [Q j])"^
          "{t = [Q (j-1)]; [Q (j-1)] = [Q j]; [Q j] = t; j = (j-1)}"^
          "{j = 0}};"^
     "write second;"^
     "i = (i+1)};"^
   "i = 0; while (n - i) {write [Q i]; i = (i+1)}"
let result5a = interpret test5a [7; 13; 11; 4; 8; 10; 17; 5]
 (* 
   val result5a : outputStream =
      [11; 13; 11; 8; 8; 8; 5; 4; 5; 8; 10; 11; 13; 17]
 *)

  (* MATRIX MULTIPLICATION: 2x3 times 3x2 *) 
     (* cij = sum_k aik bkj *)
let test6 = "array A[2,3]; array B[3,2]; array C[2,2]; variable i; variable j; variable k; i = 0; while (2 - i) {k = 0; while (3 - k) {read [[A i] k]; k = (k + 1)}; i = (i + 1)}; k = 0; while (3 - k) {j = 0; while (2 - j) {read [[B k] j]; j = (j + 1)}; k = (k + 1)}; i = 0; while (2 - i) {j = 0; while (2 - j) {[[C i] j] = 0; k = 0; while (3 - k) {[[C i] j] = ([[C i] j] + ([[A i] k] * [[B k] j])); k = (k + 1)}; write [[C i] j]; j = (j + 1)}; i = (i + 1)}"

let result6 = interpret test6 [3; 5; 4; 2; 7; 6; 1; 9; 7; 0; 4; 2]
 (*

      3  5  4        1  9      54  35  
      2  7  6   X    7  0      75  30
                     4  2

  val result6 : outputStream = [54; 35; 75; 30]
 *)

   (* MATRIX MULTIPLICATION with ALIASING of 2nd row *)
let test6a = "array A[2,3]; array B[3,2]; array C[2,2]; alias C1 [C 1]; variable i; variable j; variable k; i = 0; while (2 - i) {k = 0; while (3 - k) {read [[A i] k]; k = (k + 1)}; i = (i + 1)}; k = 0; while (3 - k) {j = 0; while (2 - j) {read [[B k] j]; j = (j + 1)}; k = (k + 1)}; i = 0; while (2 - i) {j = 0; while (2 - j) {[[C i] j] = 0; k = 0; while (3 - k) {[[C i] j] = ([[C i] j] + ([[A i] k] * [[B k] j])); k = (k + 1)}; j = (j + 1)}; i = (i + 1)}; j = 0; while (2 - j) {write [C1 j]; j = (j+1) }"

let result6a = interpret test6a [3; 5; 4; 2; 7; 6; 1; 9; 7; 0; 4; 2]

(*
  val result6a : outputStream = [75; 30]
*)

(* ERRORS *)   

let error1a = interpret "write X" []
   (* expect: "*** ERROR: X used but not declared" *)
let error1b = interpret "expand F" []
   (* expect: "*** ERROR: F used but not declared" *)
let error2 = interpret "variable w; variable z; read w; read z" [6]
   (* expect: "*** ERROR: input stream prematurely exhausted" *)
let error3 = interpret "array M[7,0,5]; write 27" []
   (* expect: "*** ERROR: M declared with a zero bound " *)
let error4a = interpret "variable A; [A 3] = 9" []
   (* expect: "*** ERROR: a scalar considered an array" *)
let error4b = interpret "array A[7]; [[A 3] 2] = 6" []
   (* expect: "*** ERROR: a scalar considered an array" *)
let error5a = interpret "array z[5]; write z" []
   (* expect: "*** ERROR: an array considered a scalar" *)
let error5b = interpret "array B[5,7]; read [B 3]" [9]
   (* expect: "*** ERROR: an array considered a scalar" *)
let error6a = interpret "array B[6]; [B 6] = 4" []
   (* expect: "*** ERROR: index 6 but allowed range is 0..5" *)
let error6b = interpret "array C[7, 4]; [[C 3] (1-2)] = 8" []
   (* expect: "*** ERROR: index -1 but allowed range is 0..3" *)

