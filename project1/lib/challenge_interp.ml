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
    | V_Fun of (Ast.Id.t list) * Ast.Expr.t * (Ast.Id.t * t) list
    [@@deriving show]

  (* to_string v = a string representation of v (more human-readable than
   * `show`.
   *)
  let to_string (v : t) : string =
    match v with
    | V_Int n -> Int.to_string n
    | V_Bool b -> Bool.to_string b
    | V_Fun _ -> "function"

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
    match List.assoc_opt x rho with
    | Some v -> v
    | None -> raise (UnboundVariable x)
  
  let fun_lookup (rho : t) (f : Ast.Id.t) : Value.t = 
    match List.assoc_opt f rho with
    | Some v -> v
    | None -> raise (UndefinedFunction f) 
    (*danner's test didn't accept when it was undefined function, so 
    we need to make two different ones for fun look up and -> functions *)

  (*  update ρ x v = ρ{x → v}.
   *)
  let update (rho : t) (x : Ast.Id.t) (v : Value.t) : t =
    (x, v) :: List.remove_assoc x rho
  
    (* Need to fix *)
  let fun_update (rho : t) (f : Ast.Id.t) (args : Ast.Id.t list) (e : Ast.Expr.t) (f_env : t): t = 
    (f, Value.V_Fun (args, e, f_env)):: List.remove_assoc f rho
  
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
  | (Ast.Expr.Div, Value.V_Int _, Value.V_Int 0) -> raise (TypeError "division by zero")
  | (Ast.Expr.Mod, Value.V_Int _, Value.V_Int 0) -> raise (TypeError "modulo by zero")
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

 (* arg_match takes the arguments of a function (held in the environment) and 
 matches them to a val list (made by evalling all the expression in the call) *)

 (* so if there are more arguments in the function than there are evaluated, we pass 
  thru a new function that has that number of arguments? *)

(* let rec arg_match (arg_env : Env.t) (xs : Ast.Id.t list) (args : Value.t list) (f_env : Env.t) : Env.t = 
    match (xs, args) with
    | (_, []) -> Env.join arg_env f_env
    | ([], _) -> raise (TypeError "too many args")
    (* | (y::ys, []) -> match y with
                  | V_Fun (args, e, f_env) -> 
                  |_ -> failwith "too few arg" *)
    | (y::ys, b::bs) -> (arg_match arg_env ys bs (Env.update f_env y b)) *)
  
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
    | _-> raise (TypeError "Invalid type "))
  | Ast.Expr.Call (f, exprs) -> 
    let valf =
      (match f with
        | Ast.Expr.Var x -> Env.fun_lookup rho x
        |_-> eval rho f)
    in
    (match valf with
      | Value.V_Fun (ids, e, f_env) -> 
        let vals = List.map (eval rho) exprs in (* here we evaluate expression list into val list *)
        let rec arg_match (xs : Ast.Id.t list) (args : Value.t list) (f_env : Env.t) : Value.t = 
          (match (xs, args) with
            | (remains, []) -> Value.V_Fun (remains, e, f_env)
            | ([], _) -> raise (TypeError "too many args")
            | (y::ys, b::bs) -> arg_match ys bs (Env.update f_env y b))
        in
        (match (arg_match ids vals f_env) with
          | Value.V_Fun ([], e, f_env) -> eval (Env.join rho f_env) e
          | v -> v
        )
      | _-> raise (TypeError "failure")) 
    (* (match f with
      | Ast.Expr.Var x -> (match (Env.fun_lookup rho x) with
                            | Value.V_Fun (args, e, f_env) -> 
                              (let rec val_list (exprs: Ast.Expr.t list) : Value.t list = 
                                (match exprs with 
                                  |[] -> []
                                  |b::bs -> (eval rho b) :: (val_list bs))
                                in
                                eval (arg_match rho args (val_list exprs) f_env) e)
                            |_ -> raise (TypeError "failure"))
      | Ast.Expr.Num _ -> raise (TypeError "failure")
      | Ast.Expr.Bool _ -> raise (TypeError "failure")
      | Ast.Expr.Unop (_, _) -> raise (TypeError "failure")
      | Ast.Expr.Binop (_, _, _) -> raise (TypeError "failure")
      | Ast.Expr.If (e_ok, e0, e1) -> (match (eval rho (Ast.Expr.If (e_ok, e0, e1))) with
                                    | Value.V_Fun (args, e, f_env) -> (let rec val_list (exprs: Ast.Expr.t list) : Value.t list = 
                                        (match exprs with 
                                          |[] -> []
                                          |b::bs -> (eval rho b) :: (val_list bs))
                                        in
                                        eval (arg_match rho args (val_list exprs) f_env) e)
                                    |_-> raise (TypeError "failure"))
      | Ast.Expr.Let (x, e0, e1) -> (match (eval rho (Ast.Expr.Let (x, e0, e1))) with
                                    | Value.V_Fun (args, e, f_env) -> (let rec val_list (exprs: Ast.Expr.t list) : Value.t list = 
                                        (match exprs with 
                                          |[] -> []
                                          |b::bs -> (eval rho b) :: (val_list bs))
                                        in
                                        eval (arg_match rho args (val_list exprs) f_env) e)
                                    |_-> raise (TypeError "failure"))
      | Ast.Expr.Call (f, xs) -> (match (eval rho (Ast.Expr.Call (f, xs))) with
                                    | Value.V_Fun (args, e, f_env) -> (let rec val_list (exprs: Ast.Expr.t list) : Value.t list = 
                                        (match exprs with 
                                          |[] -> []
                                          |b::bs -> (eval rho b) :: (val_list bs))
                                        in
                                        eval (arg_match rho args (val_list exprs) f_env) e)
                                    |_-> raise (TypeError "failure"))
      | Ast.Expr.Fun (args, e) -> (match (eval rho (Ast.Expr.Fun (args, e))) with
                                    | Value.V_Fun (args, e, f_env) -> (let rec val_list (exprs: Ast.Expr.t list) : Value.t list = 
                                        (match exprs with 
                                          |[] -> []
                                          |b::bs -> (eval rho b) :: (val_list bs))
                                        in
                                        eval (arg_match rho args (val_list exprs) f_env) e)
                                    |_-> raise (TypeError "failure"))) *)
  | Ast.Expr.Fun (xs, e) -> Value.V_Fun (xs, e, rho)
    (* Need to be able to incorporate environments for when there are fewer 
    arguments than parameters *)
    (* eval (assign_vals xs) e *)


let rec def_funks (rho: Env.t) (fds: Ast.Script.fundef list) : Env.t =
    match fds with
      | [] -> rho
      | (f, x, v)::xs -> let rho' = def_funks (Env.fun_update rho f x v rho) xs in
          Env.fun_update rho' f x v rho'
      (* There is some issue here because functions cannot call themselves *)

let exec (p : Ast.Script.t) : Value.t =
  match p with
    | Ast.Script.Pgm (fundefs, exp) -> 
        let rho = def_funks Env.empty fundefs in
        eval rho exp
    

