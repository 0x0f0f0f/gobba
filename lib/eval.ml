open Types
open Env
open Errors
module T = ANSITerminal

(** Numerical Primitives *)

let integer_sum (x, y) = match (x, y) with
    | EvtInt(a), EvtInt(b) -> EvtInt(a + b)
    | _, _ -> raise (TypeError "type mismatch in arithmetical operation")

let integer_sub (x, y) = match (x, y) with
    | EvtInt(a), EvtInt(b) -> EvtInt(a - b)
    | _, _ -> raise (TypeError "type mismatch in arithmetical operation")

let integer_mult (x, y) = match (x, y) with
    | EvtInt(a), EvtInt(b) -> EvtInt(a * b)
    | _, _ -> raise (TypeError "type mismatch in arithmetical operation")

let equals (x, y) = match (x, y) with
    | EvtInt(a), EvtInt(b) -> EvtBool(a = b)
    | EvtBool(a), EvtBool(b) -> EvtBool(a = b)
    | _, _ -> raise (TypeError "type mismatch in comparison")

let greater (x, y) = match (x, y) with
    | EvtInt(a), EvtInt(b) -> EvtBool(a > b)
    | EvtBool(a), EvtBool(b) -> EvtBool(a > b)
    | _, _ -> raise (TypeError "type mismatch in comparison")

let less (x, y) = match (x, y) with
    | EvtInt(a), EvtInt(b) -> EvtBool(a < b)
    | EvtBool(a), EvtBool(b) -> EvtBool(a < b)
    | _, _ -> raise (TypeError "type mismatch in comparison")


(** Boolean primitives *)

let bool_and (x, y) = match (x, y) with
    | EvtBool(a), EvtBool(b) -> EvtBool(a && b)
    | _, _ -> raise (TypeError "type mismatch in boolean operation")

let bool_or (x, y) = match (x, y) with
    | EvtBool(a), EvtBool(b) -> EvtBool(a || b)
    | _, _ -> raise (TypeError "type mismatch in boolean operation")

let bool_not x = match x with
    | EvtBool(a) -> EvtBool(not a)
    | _ -> raise (TypeError "type mismatch in boolean operation")

(** Evaluate an expression in an environment *)
let rec eval (e: expr) (env: env_type) (n: stackframe) vb : evt =
    let n = push_stack n e in
    let depth = (match n with
        | StackValue(d, _, _) -> d
        | EmptyStack -> 0) in
    if vb then print_message ~color:T.Blue ~loc:(Nowhere)
        "Reduction at depth" "%d\nExpression:\n%s" depth (show_expr e)
    else ();
    let evaluated = (match e with
    | Unit -> EvtUnit
    | Integer n -> EvtInt n
    | Boolean b -> EvtBool b
    | Symbol x -> lookup env x n vb
    | List x -> EvtList (eval_list x env n vb)
    | Tail l -> (match (eval l env n vb) with
        | EvtList(ls) -> (match ls with
            | [] -> raise (ListError "empty list")
            | _::r -> EvtList r)
        | _ -> raise (ListError "not a list"))
    | Head l -> (match (eval l env n vb) with
        | EvtList(ls) -> (match ls with
            | [] -> raise (ListError "empty list")
            | v::_ -> v )
        | _ -> raise (ListError "not a list"))
    | Cons(x, xs) -> (match (eval xs env n vb) with
        | EvtList(ls) -> (match ls with
            | [] -> EvtList([(eval x env n vb)])
            | lss -> EvtList((eval x env n vb)::lss))
        | _ -> raise (ListError "not a list"))
    | Sum (x,y) -> integer_sum (eval x env n vb, eval y env n vb)
    | Sub (x,y) -> integer_sub (eval x env n vb, eval y env n vb)
    | Mult (x,y) -> integer_mult (eval x env n vb, eval y env n vb)
    | Eq (x, y) -> equals (eval x env n vb, eval y env n vb)
    | Gt (x, y) -> greater (eval x env n vb, eval y env n vb)
    | Lt (x, y) -> less (eval x env n vb, eval y env n vb)
    (* Boolean operations *)
    | And (x, y) -> bool_and (eval x env n vb, eval y env n vb)
    | Or (x, y) -> bool_or (eval x env n vb, eval y env n vb)
    | Not x -> bool_not (eval x env n vb)
    | IfThenElse (guard, first, alt) ->
        let g = eval guard env n vb in
        (match g with
        | EvtBool true -> eval first env n vb
        | EvtBool false -> eval alt env n vb
        | _ -> raise (TypeError "conditional statement guard is not boolean"))
    | Let (ident, value, body) ->
        eval body (bind env ident (AlreadyEvaluated (eval value env n vb))) n vb
    | Letlazy (ident, value, body) ->
        eval body (bind env ident (LazyExpression value)) n vb
    | Letrec (ident, value, body) ->
        (match value with
            | Lambda (params, fbody) ->
                let rec_env = (bind env ident
                    (AlreadyEvaluated (RecClosure(ident, params, fbody, env))))
                in eval body rec_env n vb
            | _ -> raise (TypeError "Cannot define recursion on non-functional values"))
    | Letreclazy (ident, value, body) ->
        (match value with
            | Lambda (_, _) ->
                let rec_env = (bind env ident (LazyExpression value))
                in eval body rec_env n vb
            | _ -> raise (TypeError "Cannot define recursion on non-functional values"))
    | Lambda (params,body) -> Closure(params, body, env)
    | Apply(f, params) ->
        let closure = eval f env n vb in
        (match closure with
        | Closure(args, body, decenv) -> (* Use static scoping *)
            let evaluated_params = List.map (fun x -> AlreadyEvaluated (eval x env n vb)) params in
            let application_env = bindlist decenv args evaluated_params in
            eval body application_env n vb
        | RecClosure(name, args, body, decenv) ->
            let evaluated_params = List.map (fun x -> AlreadyEvaluated (eval x env n vb)) params in
            let rec_env = (bind decenv name (AlreadyEvaluated closure)) in
            let application_env = bindlist rec_env args evaluated_params in
            eval body application_env n vb
        | _ -> raise (TypeError "Cannot apply a non functional value")))
    in
    if vb then print_message ~color:T.Cyan ~loc:(Nowhere)
        "Evaluates to at depth" "%d\n%s\n" depth (show_evt evaluated)
    else ();
    evaluated;
and eval_list (l: list_pattern) (env: env_type) (n: stackframe) vb: evt list =
    match l with
        | EmptyList -> []
        | ListValue(x, xs) -> (eval x env n vb)::(eval_list xs env n vb)
(* Search for a value in an environment *)
and lookup (env: env_type) (ident: ide) (n: stackframe) vb : evt =
    if ident = "" then failwith "invalid identifier" else
    match env with
    | [] -> raise (UnboundVariable ident)
    | (i, LazyExpression e) :: env_rest -> if ident = i then eval e env n vb
        else lookup env_rest ident n vb
    | (i, AlreadyEvaluated e) :: env_rest -> if ident = i then e else
        lookup env_rest ident n vb
