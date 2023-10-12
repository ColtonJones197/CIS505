(*

Grammar:

  B ::= \empty
     |  '{' B '}' B
     |  '[' B ']' B

*)

exception Error of string

type tokenT =
   OpenCurlyT
 | CloseCurlyT
 | OpenSquareT
 | CloseSquareT

let printToken token = match token with
 | OpenCurlyT -> "{"
 | CloseCurlyT -> "}"
 | OpenSquareT -> "["
 | CloseSquareT -> "]"

let rec lexer str =
  if str = ""
   then []
   else let ch = String.get str 0 
        and str1 = String.sub str 1 (String.length str - 1) in
   match ch with
   | ' ' -> lexer str1
   | '{' -> OpenCurlyT :: (lexer str1)
   | '}' -> CloseCurlyT :: (lexer str1)
   | '[' -> OpenSquareT :: (lexer str1)
   | ']' -> CloseSquareT :: (lexer str1)
   | _ -> raise (Error "unknown symbol")

type balanced =
   EmptyB
 | CurlyB of balanced * balanced
 | SquareB of balanced * balanced 

let expect token tokens = match tokens with
 | [] -> raise (Error ("'"^(printToken token)^"' expected but input exhausted"))
 | (token1 :: tokens') -> 
     if token = token1
     then tokens'
     else raise (Error ("'"^(printToken token)^"' expected but '"^
                        (printToken token1)^"' seen"))

let rec parseB tokens = match tokens with
 | OpenCurlyT :: tokens0 ->
     let (b1, tokens1) = parseB tokens0 in
     let (b2, tokens2) = parseB tokens0 (* MODIFY! *) in
    (CurlyB (b1,b2), tokens2)
 | OpenSquareT :: tokens0 ->
     let (b1, tokens1) = parseB tokens0 in
     let (b2, tokens2) = parseB tokens0 (* MODIFY! *) in
    (SquareB (b1,b2), tokens2)
 | _ -> (EmptyB, tokens)

let parse tokens = match parseB tokens with
  | (b, []) -> b
  | (_, (token :: _)) ->
          raise (Error ("unused input, starting with "^
                           (printToken token)^"'"))

let rec depth b = 0 (* MODIFY! *)

let run input_string =
  try depth (parse (lexer input_string)) with 
  | (Error msg) -> (print_string ("Error: "^msg^"\n"); 0)

let error1 = "{}"   (* MODIFY such that "run error1 ;;" gives
                        "Error: '}' expected but input exhausted"  *)

let error2 = "{}"   (* MODIFY such that "run error2 ;;" gives
                        "Error: ']' expected but '}' seen"         *)

let error3 = "{}";; (* MODIFY such that "run error3 ;;" gives
                        "Error: unused input, starting with '}'"   *) 



