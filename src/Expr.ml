(* Simple expressions: syntax and semantics *)

(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT 
             
(* The type for the expression. Note, in regular OCaml there is no "@type..." 
   notation, it came from GT. 
*)
@type expr =
  (* integer constant *) | Const of int
  (* variable         *) | Var   of string
  (* binary operator  *) | Binop of string * expr * expr with show

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

(* An example of a non-trivial state: *)                                                   
let s = update "x" 1 @@ update "y" 2 @@ update "z" 3 @@ update "t" 4 empty

(* Some testing; comment this definition out when submitting the solution.
let _ =
  List.iter
    (fun x ->
       try  Printf.printf "%s=%d\n" x @@ s x
       with Failure s -> Printf.printf "%s\n" s
    ) ["x"; "a"; "y"; "z"; "t"; "b"]
 *)

(* Expression evaluator

     val eval : state -> expr -> int
 
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
	 
