open GT       
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
type config = int list * Syntax.Stmt.config

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
    | ST x -> (tl stack, (Syntax.Expr.update x (hd stack) state, input, output))
    | BINOP operate -> 
        let first :: second :: last = stack in
        ((Syntax.Expr.token_to_binop operate) (second) (first) :: (last), (state, input, output))

let rec eval config programm = 
	match programm with
	| x :: xl -> eval (nextInstruction config x) xl
	| [] -> config

(* Top-level evaluation

     val run : int list -> prg -> int list

   Takes an input stream, a program, and returns an output stream this program calculates
*)
let run i p = let (_, (_, _, o)) = eval ([], (Syntax.Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Syntax.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)

let rec compileExpr expr = 
		match expr with
		| Syntax.Expr.Const c -> [CONST c]
		| Syntax.Expr.Var x -> [LD x]
		| Syntax.Expr.Binop (operate, left, right) -> compileExpr left @ compileExpr right @ [BINOP operate]

let rec compile stmt = 
	match stmt with
		| Syntax.Stmt.Read x -> [READ; ST x]
		| Syntax.Stmt.Write x -> compileExpr x @ [WRITE]
		| Syntax.Stmt.Assign (x, y) -> compileExpr y @ [ST x]
		| Syntax.Stmt.Seq (left, right) -> compile left @ compile right;;