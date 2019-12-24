open Types
open Env
open Errors
open Util
open Typecheck
open Primitives

module T = ANSITerminal

(** Numerical Primitives *)

let int_binop (x, y) (op: int -> int -> int) = match (x, y) with
  | EvtInt(a), EvtInt(b) -> EvtInt(op a b)
  | _, _ -> raise (TypeError "type mismatch in arithmetical operation")

let bool_binop (x, y) (op: bool -> bool -> bool) = match (x, y) with
  | EvtBool(a), EvtBool(b) -> EvtBool(op a b)
  | _, _ -> raise (TypeError "type mismatch in boolean operation")

let bool_unop x (op: bool -> bool) = match x with
  | EvtBool(a) -> EvtBool(op a)
  | _ -> raise (TypeError "type mismatch in boolean operation")

let uniqueorfail l = if dup_key_exist l then
  raise (DictError "Duplicate key in dictionary")
  else l

(** Evaluate an expression in an environment *)
let rec eval (e: expr) (env: env_type) (n: stackframe) vb : evt =
  let n = push_stack n e in
  let depth = (match n with
    | StackValue(d, _, _) -> d
    | EmptyStack -> 0) in
  (* Partially apply eval to the current stackframe, verbosity and environment *)
  let ieval = fun x -> eval x env n vb in
  if vb then print_message ~color:T.Blue ~loc:(Nowhere)
    "Reduction at depth" "%d\nExpression:\n%s" depth (show_expr e)
  else ();
  let evaluated = (match e with
  | Unit -> EvtUnit
  | Integer n -> EvtInt n
  | Boolean b -> EvtBool b
  | String s -> EvtString s
  | Symbol x -> lookup env x ieval
  | List x -> EvtList (List.map ieval x)
  | Cons (x, xs) ->
    let ls = unpack_list (ieval xs) in
    (match ls with
      | [] -> EvtList([(ieval x)])
      | lss -> EvtList((ieval x)::lss))
  (* Dictionaries and operations *)
  | Dict(l) ->
    let el = uniqueorfail (List.map (fun (x,y) -> isvalidkey (ieval x, ieval y)) l) in
    EvtDict el
  | Sum   (x, y) ->   int_binop   (ieval x, ieval y)  (+)
  | Sub   (x, y) ->   int_binop   (ieval x, ieval y)  (-)
  | Mult  (x, y) ->   int_binop   (ieval x, ieval y)  ( * )
  | And   (x, y) ->   bool_binop  (ieval x, ieval y)  (&&)
  | Or    (x, y) ->   bool_binop  (ieval x, ieval y)  (||)
  | Not   x      ->   bool_unop   (ieval x)           (not)
  | Eq    (x, y) ->   EvtBool(compare_evt (ieval x) (ieval y) = 0)
  | Gt    (x, y) ->   EvtBool(compare_evt (ieval x) (ieval y) > 0)
  | Lt    (x, y) ->   EvtBool(compare_evt (ieval x) (ieval y) < 0)
  | IfThenElse (guard, first, alt) ->
    let g = unpack_bool (ieval guard) in
    if g then ieval first else ieval alt
  | Let (assignments, body) ->
    let evaluated_assignments = List.map
      (fun (_, value) -> AlreadyEvaluated (ieval value)) assignments
    and identifiers = fstl assignments in
    let new_env = bindlist env identifiers evaluated_assignments in
    eval body new_env n vb
  | Letlazy (assignments, body) ->
    let identifiers = fstl assignments in
    let new_env = bindlist env identifiers
      (List.map (fun (_, value) -> LazyExpression value) assignments) in
    eval body new_env n vb
  | Letrec (ident, value, body) ->
    (match value with
      | Lambda (form_params, fbody) ->
        let rec_env = (bind env ident
          (AlreadyEvaluated (RecClosure(ident, form_params, fbody, env))))
        in eval body rec_env n vb
      | _ -> raise (TypeError "Cannot define recursion on non-functional values"))
  | Letreclazy (ident, value, body) ->
    (match value with
      | Lambda (_, _) ->
        let rec_env = (bind env ident (LazyExpression value))
        in eval body rec_env n vb
      | _ -> raise (TypeError "Cannot define recursion on non-functional values"))
  | Lambda (form_params,body) -> Closure(form_params, body, env)
  (* Special Primitives *)
  (* Map a function over an iterable structure *)
  | Apply (Symbol "map", act_params) ->
    let (f, s) = (match act_params with
      | [f; s] -> (f, s)
      | _ -> raise WrongPrimitiveArgs) in
    let ef = ieval f and es = ieval s in
    typecheck ef "fun";
    (match es with
      | EvtList x ->
        EvtList(List.map (fun x -> applyfun ef [AlreadyEvaluated x] n vb) x)
      | EvtDict d ->
        let (keys, values) = unzip d in
        EvtDict(zip keys (List.map (fun x -> applyfun ef [AlreadyEvaluated x] n vb) values))
      | _ -> failwith "Value is not iterable")
  (* Function Application *)
  | Apply(f, act_params) ->
    let applyaux f act_params =
      let closure = ieval f in
      let evaluated_params = List.map (fun x -> AlreadyEvaluated (ieval x)) act_params in
      applyfun closure evaluated_params n vb in
    (match f with
      | Symbol s -> if key_exist s primitives_table
        then (get_key_val s primitives_table) (List.map ieval act_params)
        else applyaux f act_params
      | _ -> applyaux f act_params)
  (* Eval a sequence of expressions but return the last *)
  | Sequence(exprl) ->
    let rec unroll el = (match el with
    | [] -> failwith "fatal: empty command sequence"
    | x::[] -> ieval x
    | x::xs -> (let _ = ieval x in unroll xs)) in unroll exprl
  (* Pipe two functions together, creating a new function
     That uses the first functions's result as the second's first argument *)
  | Pipe(e1, e2) ->
    (* Convert a list of identifiers to a list of symbols *)
    let syml l = List.map (fun x -> Symbol x) l in
    let f1 = ieval e1 and f2 = ieval e2 in
    typecheck f2 "fun";
    let (_, params1, _, _) = unpack_anyfun f1 in
    Closure(params1, Apply(e2, [Apply(e1, syml params1)]), env))
  in
  if vb then print_message ~color:T.Cyan ~loc:(Nowhere)
    "Evaluates to at depth" "%d\n%s\n" depth (show_evt evaluated)
  else ();
  evaluated;
(* Search for a value in an environment *)
and lookup (env: env_type) (ident: ide) ieval : evt =
  if ident = "" then failwith "invalid identifier" else
  match env with
  | [] -> raise (UnboundVariable ident)
  | (i, LazyExpression e) :: env_rest -> if ident = i then ieval e
    else lookup env_rest ident ieval
  | (i, AlreadyEvaluated e) :: env_rest -> if ident = i then e else
    lookup env_rest ident ieval
and applyfun closure evaluated_params n vb =
  let p_length = List.length evaluated_params in
  match closure with
    | Closure(form_params, body, decenv) -> (* Use static scoping *)
      if (List.compare_lengths form_params evaluated_params) > 0 then (* curry *)
        let applied_env = bindlist decenv (take p_length form_params) evaluated_params in
        Closure((drop p_length form_params), body, applied_env)
      else  (* apply the function *)
        let application_env = bindlist decenv form_params evaluated_params in
        eval body application_env n vb
    (* Apply a recursive function *)
    | RecClosure(name, form_params, body, decenv) ->
      let rec_env = (bind decenv name (AlreadyEvaluated closure)) in
      if (List.compare_lengths form_params evaluated_params) > 0 then (* curry *)
        let applied_env = bindlist rec_env (take p_length form_params) evaluated_params in
        RecClosure(name, (drop p_length form_params), body, applied_env)
      else  (* apply the function *)
        let application_env = bindlist rec_env form_params evaluated_params in
        eval body application_env n vb
    | _ -> raise (TypeError "Cannot apply a non functional value")