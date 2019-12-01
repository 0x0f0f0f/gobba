open Types
open Env

(** Numerical Primitives *)

let integer_sum (x, y) = match (x, y) with
    | Int(a), Int(b) -> Int(a + b)
    | _, _ -> failwith "wrong type in arithmetical operation"

let integer_sub (x, y) = match (x, y) with
    | Int(a), Int(b) -> Int(a - b)
    | _, _ -> failwith "wrong type in arithmetical operation"

let integer_mult (x, y) = match (x, y) with
    | Int(a), Int(b) -> Int(a * b)
    | _, _ -> failwith "wrong type in arithmetical operation"

let equals (x, y) = match (x, y) with
    | Int(a), Int(b) -> Bool(a = b)
    | Bool(a), Bool(b) -> Bool(a = b)
    | _, _ -> failwith "type mismatch in equality"

(** Boolean primitives *)

let bool_and (x, y) = match (x, y) with
    | Bool(a), Bool(b) -> Bool(a && b)
    | _, _ -> failwith "wrong type in boolean operation"

let bool_or (x, y) = match (x, y) with
    | Bool(a), Bool(b) -> Bool(a && b)
    | _, _ -> failwith "wrong type in boolean operation"

let bool_not x = match x with
    | Bool(a) -> Bool(not a)
    | _ -> failwith "wrong type in boolean operation"

(** Evaluate an expression in an environment *)
let rec eval (e: expr) (env: env_type) : evt = match e with
    | Integer n -> Int n
    | Boolean b -> Bool b
    | Symbol x -> lookup env x
    | Sum (x,y) -> integer_sum (eval x env, eval y env)
    | Sub (x,y) -> integer_sub (eval x env, eval y env)
    | Mult (x,y) -> integer_mult (eval x env, eval y env)
    | Eq (x, y) -> equals (eval x env, eval y env)
    (* Boolean operations *)
    | And (x, y) -> bool_and (eval x env, eval y env)
    | Or (x, y) -> bool_or (eval x env, eval y env)
    | Not x -> bool_not (eval x env)
    | IfThenElse (guard, first, alt) ->
        let g = eval guard env in
        (match g with
        | Bool true -> eval first env
        | Bool false -> eval alt env
        | _ -> failwith "Nonboolean guard!")
    | Let (ident, value, body) ->
        eval body (bind env ident (eval value env))
    | Lambda (params,body) -> Closure(params, body, env)
    | Apply(f, params) ->
        let closure = eval f env in
        match closure with
        | Closure(args, body, decenv) -> (* Use static scoping *)
            let evaluated_params = List.map (fun x -> eval x env) params in
            let application_env = bindlist decenv args evaluated_params in
            eval body application_env
        | _ -> failwith "Not a function!"
