(* Ocaml- interpreter.
 *
 * N. Danner
 *)

module Ast = Core_ast

(* UndefinedFunction f is raised when f is called but not defined.
 *)
exception UndefinedFunction of Ast.Id.t

(* UnboundVariable x is raised when x is used but not declared.
 *)
exception UnboundVariable of Ast.Id.t

(* TypeError s is raised when an operator or function is applied to operands
 * of the incorrect type.  s is any (hopefuly useful) message.
 *)
exception TypeError of string

(* Values.
 *)
module Value = struct
  type t = 
    | V_Int of int
    | V_Bool of bool
    [@@deriving show]

  (* to_string v = a string representation of v (more human-readable than
   * `show`.
   *)
  let to_string (v : t) : string =
    match v with
    | V_Int n -> Int.to_string n
    | V_Bool b -> Bool.to_string b
end

(* Environments.  An environment is a finite map from identifiers to values.
 * We will interchangeably treat environments as functions or sets or lists
 * of pairs in documentation.  We will use ρ as a metavariable over
 * environments.
 *)
module Env = struct
  type vars = (Ast.Id.t * Value.t) list
  (* Function name, arguments, expression *)
  type funks = ((Ast.Id.t * Ast.Id.t list) * Ast.Expr.t) list
  (*  empty = ρ, where dom ρ = ∅.
   *)
  type t = vars * funks
  let empty : t = ([], [])


  (*  lookup ρ x = ρ(x).
   *)
  let lookup (rho : t) (x : Ast.Id.t) : Value.t =
    let (vars, _) = rho in
    List.assoc x vars

  (*  update ρ x v = ρ{x → v}.
   *)
  let update (rho : t) (x : Ast.Id.t) (v : Value.t) : t =
    let (vars, funks) = rho in
    ((x, v) :: List.remove_assoc x vars, funks)
  
  let fun_update (rho:t) (f : Ast.Id.t) (x : Ast.Id.t list) (vt :Ast.Expr.t) : t =
    let (vars, funks) = rho in
    (vars, ((f, x), vt) :: List.remove_assoc (f, x) funks)
end



let unop (op : Ast.Expr.unop) (v : Value.t) : Value.t =
  match (op, v) with
  |(Ast.Expr.Not, Value.V_Bool n) -> Value.V_Bool (not n)
  |(Ast.Expr.Neg, Value.V_Int n) -> Value.V_Int (-n)
  |_ -> failwith "Unimplemented"

let binop (op : Ast.Expr.binop) (v : Value.t) (v' : Value.t) : Value.t =
  match (op, v, v') with
  | (Ast.Expr.Plus, Value.V_Int n, Value.V_Int n') -> Value.V_Int (n + n')
  | (Ast.Expr.Minus, Value.V_Int n, Value.V_Int n') -> Value.V_Int (n - n')
  | (Ast.Expr.Times, Value.V_Int n, Value.V_Int n') -> Value.V_Int (n * n')
  | (Ast.Expr.Div, Value.V_Int n, Value.V_Int n') -> Value.V_Int (n / n')
  | (Ast.Expr.Mod, Value.V_Int n, Value.V_Int n') -> Value.V_Int (n mod n')
  | (Ast.Expr.And, Value.V_Bool n, Value.V_Bool n') -> Value.V_Bool (n && n')
  | (Ast.Expr.Or, Value.V_Bool n, Value.V_Bool n') -> Value.V_Bool (n || n')
  | (Ast.Expr.Eq, Value.V_Int n, Value.V_Int n') -> Value.V_Bool (n == n')
  | (Ast.Expr.Ne, Value.V_Int n, Value.V_Int n') -> Value.V_Bool (n != n')
  | (Ast.Expr.Lt, Value.V_Int n, Value.V_Int n') -> Value.V_Bool (n < n')
  | (Ast.Expr.Gt, Value.V_Int n, Value.V_Int n') -> Value.V_Bool (n > n')
  | (Ast.Expr.Le, Value.V_Int n, Value.V_Int n') -> Value.V_Bool (n <= n')
  | (Ast.Expr.Ge, Value.V_Int n, Value.V_Int n') -> Value.V_Bool (n >= n')
  |_ -> failwith "you even more stupid idiot"


(* exec p = v, where `v` is the result of executing `p`.
 *)

 (* Write a seperate eval function to evaluate expressions 
  Like sample code, but now we have to deal with function definitions *)

let rec eval (rho : Env.t) (e : Ast.Expr.t) : Value.t =
  match e with
(*! end !*)
  | Ast.Expr.Var x -> Env.lookup rho x
  | Ast.Expr.Num n -> Value.V_Int n
  |Ast.Expr.Unop (op, e) ->
    let v = eval rho e in 
    unop op v 
(*! eval binop !*)
  | Ast.Expr.Binop (op, e, e') ->
    let v = eval rho e in
    let v' = eval rho e' in
    binop op v v'
(*! end !*)
(*! eval let !*)
  | Ast.Expr.Let (x, e', e) ->
    let v' = eval rho e' in
    eval (Env.update rho x v') e
  | Ast.Expr.If (Bool e, e0, e1) ->
    (match e with
    | true -> eval rho e0
    | false -> eval rho e1)
  |_ -> failwith "Unimplemented" 

let rec def_funks (rho: Env.t) (fds: Ast.Script.fundef list) : Env.t =
    match fds with
      | [] -> ([], [])
      | (f, x, v)::xs -> Env.fun_update (def_funks rho xs) f x v  

let exec (p : Ast.Script.t) : Value.t =
  match p with
    | Pgm (fundefs, exp) -> 
        let rho = def_funks Env.empty fundefs in
        eval rho exp
    

