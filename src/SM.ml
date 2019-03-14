open GT       
open Language
open List
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

(* Stack machine interpreter
     val eval : config -> prg -> config
   Takes a configuration and a program, and returns a configuration as a result
 *)                         
let nextInstruction (stack, (state, input, output)) instruction = 
    match instruction with
    | CONST c -> (c :: stack, (state, input, output))
    | READ -> (hd input :: stack, (state, tl input, output))
    | WRITE -> (tl stack, (state, input, output @ [hd stack]))
    | LD x -> (state x :: stack, (state, input, output))
    | ST x -> (tl stack, (Expr.update x (hd stack) state, input, output))
    | BINOP operate -> 
        let first :: second :: last = stack in
        ((Expr.token_to_binop operate) (second) (first) :: (last), (state, input, output))

let rec eval config programm = 
	match programm with
	| x :: xl -> eval (nextInstruction config x) xl
	| [] -> config

(* Top-level evaluation
     val run : prg -> int list -> int list
   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i = let (_, (_, _, o)) = eval ([], (Expr.empty, i, [])) p in o

(* Stack machine compiler
     val compile : Language.Stmt.t -> prg
   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)
let rec compile =
  let rec expr = function
  | Expr.Var   x          -> [LD x]
  | Expr.Const n          -> [CONST n]
  | Expr.Binop (op, x, y) -> expr x @ expr y @ [BINOP op]
  in
  function
  | Stmt.Seq (s1, s2)  -> compile s1 @ compile s2
  | Stmt.Read x        -> [READ; ST x]
  | Stmt.Write e       -> expr e @ [WRITE]
  | Stmt.Assign (x, e) -> expr e @ [ST x]
