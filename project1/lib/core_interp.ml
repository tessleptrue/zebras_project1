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


module Env = struct
  type var = (Ast.Id.t * Value.t) 
  type vars = var list
  (* Function name, arguments, expression *)

  type funk = (Ast.Id.t * (Ast.Id.t list * Ast.Expr.t))
  type funks = funk list
  (*  empty = ρ, where dom ρ = ∅.
   *)
  type t = vars * funks
  let empty : t = ([], [])
  [@@deriving show]

  (*  empty = ρ, where dom ρ = ∅. lookup ρ x = ρ(x). *)
  
  let lookup (rho : t) (x : Ast.Id.t) : Value.t =
    let (vs, _) = rho in
    match List.assoc_opt x vs with
    | Some v -> v
    | None -> raise (UnboundVariable x)

  let fun_lookup (rho : t) (f : Ast.Id.t): Ast.Id.t list * Ast.Expr.t =
    let (_ , fs) = rho in
    match List.assoc_opt f fs with
    | Some v -> v
    | None -> raise (UndefinedFunction f) 

  (*  update ρ x v = ρ{x → v}.
   *)
  let update (rho : t) (x : Ast.Id.t) (v : Value.t) : t =
    let (vs, fs) = rho in
    ((x, v) :: List.remove_assoc x vs, fs)
  
  let fun_update (rho:t) (f : Ast.Id.t) (x : Ast.Id.t list) (vt :Ast.Expr.t) : t =
    let (vs, fs) = rho in
    (vs, (f, (x, vt)) :: List.remove_assoc f fs)
end

(* It looks like what we need to do is copy a bunch of stuff from OCaml--, then extend to include bools
and if/then statements. Do we also need to add functions for reading scripts and programs??
*)

let unop (op : Ast.Expr.unop) (v : Value.t) : Value.t =
  match (op, v) with
  |(Ast.Expr.Not, Value.V_Bool n) -> Value.V_Bool (not n)
  |(Ast.Expr.Neg, Value.V_Int n) -> Value.V_Int (-n)
  |_  -> raise (TypeError "Invalid operand for unary operator")


let binop (op : Ast.Expr.binop) (v : Value.t) (v' : Value.t) : Value.t =
  match (op, v, v') with
  | (Ast.Expr.Plus, Value.V_Int n, Value.V_Int n') -> Value.V_Int (n + n')
  | (Ast.Expr.Minus, Value.V_Int n, Value.V_Int n') -> Value.V_Int (n - n')
  | (Ast.Expr.Times, Value.V_Int n, Value.V_Int n') -> Value.V_Int (n * n')
  | (Ast.Expr.Div, Value.V_Int n, Value.V_Int n') -> Value.V_Int (n / n')
  | (Ast.Expr.Mod, Value.V_Int n, Value.V_Int n') -> Value.V_Int (n mod n')
  | (Ast.Expr.And, Value.V_Bool n, Value.V_Bool n') -> Value.V_Bool (n && n')
  | (Ast.Expr.Or, Value.V_Bool n, Value.V_Bool n') -> Value.V_Bool (n || n')
  | (Ast.Expr.Eq, Value.V_Int n, Value.V_Int n') -> Value.V_Bool (n = n')
  | (Ast.Expr.Ne, Value.V_Int n, Value.V_Int n') -> Value.V_Bool (n != n')
  | (Ast.Expr.Lt, Value.V_Int n, Value.V_Int n') -> Value.V_Bool (n < n')
  | (Ast.Expr.Gt, Value.V_Int n, Value.V_Int n') -> Value.V_Bool (n > n')
  | (Ast.Expr.Le, Value.V_Int n, Value.V_Int n') -> Value.V_Bool (n <= n')
  | (Ast.Expr.Ge, Value.V_Int n, Value.V_Int n') -> Value.V_Bool (n >= n')
  |_ -> raise (TypeError "Unsupported expression")


(* exec p = v, where `v` is the result of executing `p`.
 *)

let rec arg_match (arg_env : Env.t) (xs : Ast.Id.t list) (args : Value.t list) : Env.t = 
    match (xs, args) with
    | ([], []) -> arg_env
    | ([], _) -> failwith "too many args"
    | (_, []) -> failwith "too few args"
    | (y::ys, b::bs) -> (arg_match (Env.update arg_env y b) ys bs )

 (* Write a seperate eval function to evaluate expressions 
  Like sample code, but now we have to deal with function definitions *)
let rec eval (rho : Env.t) (e : Ast.Expr.t) : Value.t =
  match e with
(*! end !*)
  | Ast.Expr.Var x -> Env.lookup rho x
  | Ast.Expr.Num n -> Value.V_Int n
  | Ast.Expr.Bool b -> Value.V_Bool b
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
  | Ast.Expr.If (e, e0, e1) ->
    (match (eval rho e) with
    | Value.V_Bool true -> eval rho e0
    | Value.V_Bool false -> eval rho e1
    | _-> failwith "stupid" )
  | Ast.Expr.Call (f, args) -> 
    let (xs, e) = Env.fun_lookup rho f in
      let rec val_list (args: Ast.Expr.t list) : Value.t list = 
        (match args with 
          |[] -> []
          |b::bs -> (eval rho b) :: (val_list bs))
         in
         eval (arg_match rho xs (val_list args)) e


let rec def_funks (rho: Env.t) (fds: Ast.Script.fundef list) : Env.t =
    match fds with
      | [] -> rho
      | (f, x, v)::xs -> def_funks (Env.fun_update rho f x v) xs
  
        (* Env.fun_update (def_funks happy xs) f x v   *)

let exec (p : Ast.Script.t) : Value.t =
  match p with
    | Pgm (fundefs, exp) -> 
        let rho = def_funks Env.empty fundefs in
        eval rho exp
    

