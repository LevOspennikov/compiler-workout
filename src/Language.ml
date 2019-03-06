(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap.Combinators
       
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)
                                                            
    (* State: a partial map from variables to integer values. *)
    type state = string -> int 

    (* Empty state: maps every variable into nothing. *)
    let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

    (* Update: non-destructively "modifies" the state s by binding the variable x 
      to value v and returns the new state.
    *)
    let update x v s = fun y -> if x = y then v else s y

    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
    *)
    let to_int x = if x then 1 else 0
    let to_bool_int x = if x <> 0 then 1 else 0

    let token_to_binop s = 
        match s with
        | "+"  -> ( + )
        | "-"  -> ( - )
        | "*"  -> ( * )
        | "/"  -> ( / )
        | "%"  -> ( mod )
        | ">"  -> fun l r -> to_int (l > r)
        | "<"  -> fun l r -> to_int (l < r)
        | ">=" -> fun l r -> to_int (l >= r)
        | "<=" -> fun l r -> to_int (l <= r)
        | "==" -> fun l r -> to_int (l == r)
        | "!=" -> fun l r -> to_int (l != r)
        | "&&" -> fun l r -> ((to_bool_int l) land (to_bool_int r))
        | "!!" -> fun l r -> ((to_bool_int l) lor (to_bool_int r))
        | _ -> failwith (Printf.sprintf "Unsupported operator %s" s)

    let rec eval s e =
        match e with
        | Const value -> value
        | Var varname -> s varname
        | Binop (token, l, r) -> (token_to_binop token) (eval s l) (eval s r)  

    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
   
    *)
    ostap (
      parse: empty {failwith "Not implemented yet"}
    )

  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator

          val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
<<<<<<< HEAD:src/Syntax.ml
    let rec eval (state, input, output) stat = 
    match stat with
      | Read name -> (Expr.update name (hd input) state, tl input, output)
      | Write expr -> (state, input, Expr.eval state expr :: output)
      | Assign (name, expr) -> (Expr.update name (Expr.eval state expr) state, input, output)
      | Seq (left, right) -> eval (eval (state, input, output) left) right
                                                         
=======
    let eval _ = failwith "Not implemented yet"

    (* Statement parser *)
    ostap (
      parse: empty {failwith "Not implemented yet"}
    )
      
>>>>>>> 679ad67411e29a42b11bdf1cefb9a6211038a63b:src/Language.ml
  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval p i =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o
