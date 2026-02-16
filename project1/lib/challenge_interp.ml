(* Ocaml- interpreter.
 *
 * N. Danner
 *)

module Ast = Challenge_ast

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
    | V_Fun of (Ast.Id.t list) * Ast.Expr.t
    [@@deriving show]

  (* to_string v = a string representation of v (more human-readable than
   * `show`.
   *)
  let to_string (v : t) : string =
    match v with
    | V_Int n -> Int.to_string n
    | V_Bool b -> Bool.to_string b
    | V_Fun (_, _) -> "fail whoopsies"
      (* let rec args_to_string (arguments : Ast.Var.t list) : string = 
                                  (match arguments with
                                    |[] -> ""
                                    |x::xs -> (to_string x)^(args_to_string xs))
    in *)

end

(* Environments.  An environment is a finite map from identifiers to values.
 * We will interchangeably treat environments as functions or sets or lists
 * of pairs in documentation.  We will use ρ as a metavariable over
 * environments.
 *)
module Env = struct

  type t = (Ast.Id.t * Value.t) list
  [@@deriving show]

  (*  empty = ρ, where dom ρ = ∅.
   *)
  let empty : t = []

  (* from_list xsvs = xsvs.
   *)
  let from_list : t -> t = fun rho -> rho

  (* join ρ₀ ρ₁ = ρ, where:
   *   dom ρ = dom ρ₀ ∪ dom ρ₁
   *   ρ(x) = ρ₀(x), x ∈ dom ρ₀ - dom ρ₁
   *          ρ₁(x), x ∈ dom ρ₁.
   *)
  let join (rho0 : t) (rho1 : t) : t =
    List.append (
      List.filter(
        fun (x, _) -> not @@ List.mem_assoc x rho1
      ) rho0
    ) rho1

  (*  lookup ρ x = ρ(x).
   *)
  let lookup (rho : t) (x : Ast.Id.t) : Value.t = 
    List.assoc x rho

  (*  update ρ x v = ρ{x → v}.
   *)
  let update (rho : t) (x : Ast.Id.t) (v : Value.t) : t =
    (x, v) :: List.remove_assoc x rho
  
  let fun_update (rho : t) (f : Ast.Id.t) (args : Ast.Id.t list) (e : Ast.Expr.t) : t = 
    (f, Value.V_Fun (args, e)):: List.remove_assoc f rho
  
end

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
  
(* let rec assign_vals (u_args : Id.t list) : Value.t list =
    match u_args with
    |[] -> []
    |x::xs -> (lookup rho x) :: (assign_vals xs) *)

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
  | Ast.Expr.Call (f, exprs) -> 
    (match f with
      | Ast.Expr.Var x -> (match (Env.lookup rho x) with
                            | Value.V_Fun (args, e) -> 
                              (let rec val_list (exprs: Ast.Expr.t list) : Value.t list = 
                                (match exprs with 
                                  |[] -> []
                                  |b::bs -> (eval rho b) :: (val_list bs))
                                in
                                eval (arg_match rho args (val_list exprs)) e)
                            |_ -> failwith "don't be stupid")
      | Ast.Expr.Num _ -> failwith "no numbers please"
      | Ast.Expr.Bool _ -> failwith "no bools please"
      | Ast.Expr.Unop (_, _) -> failwith "no unops please"
      | Ast.Expr.Binop (_, _, _) -> failwith "no binops please"
      | Ast.Expr.If (e_ok, e0, e1) -> (match (eval rho (Ast.Expr.If (e_ok, e0, e1))) with
                                    | Value.V_Fun (args, e) -> (let rec val_list (exprs: Ast.Expr.t list) : Value.t list = 
                                        (match exprs with 
                                          |[] -> []
                                          |b::bs -> (eval rho b) :: (val_list bs))
                                        in
                                        eval (arg_match rho args (val_list exprs)) e)
                                    |_-> failwith "don't be stupid here either")
      | Ast.Expr.Let (x, e0, e1) -> (match (eval rho (Ast.Expr.Let (x, e0, e1))) with
                                    | Value.V_Fun (args, e) -> (let rec val_list (exprs: Ast.Expr.t list) : Value.t list = 
                                        (match exprs with 
                                          |[] -> []
                                          |b::bs -> (eval rho b) :: (val_list bs))
                                        in
                                        eval (arg_match rho args (val_list exprs)) e)
                                    |_-> failwith "don't be stupid here either")
      | Ast.Expr.Call (f, xs) -> (match (eval rho (Ast.Expr.Call (f, xs))) with
                                    | Value.V_Fun (args, e) -> (let rec val_list (exprs: Ast.Expr.t list) : Value.t list = 
                                        (match exprs with 
                                          |[] -> []
                                          |b::bs -> (eval rho b) :: (val_list bs))
                                        in
                                        eval (arg_match rho args (val_list exprs)) e)
                                    |_-> failwith "don't be stupid here either")
      | Ast.Expr.Fun (args, e) -> (match (eval rho (Ast.Expr.Fun (args, e))) with
                                    | Value.V_Fun (args, e) -> (let rec val_list (exprs: Ast.Expr.t list) : Value.t list = 
                                        (match exprs with 
                                          |[] -> []
                                          |b::bs -> (eval rho b) :: (val_list bs))
                                        in
                                        eval (arg_match rho args (val_list exprs)) e)
                                    |_-> failwith "don't be stupid here either"))
  | Ast.Expr.Fun (xs, e) -> Value.V_Fun (xs, e)
    (* eval (assign_vals xs) e *)


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
    

