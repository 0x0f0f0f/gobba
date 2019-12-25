open Types
open Env
open Errors
open Util
open Typecheck

module T = ANSITerminal

(** Numerical Primitives *)

let int_binop (x, y) (op: int -> int -> int) =
  let a = unpack_int x and b = unpack_int y in EvtInt(op a b)

let bool_binop (x, y) (op: bool -> bool -> bool) =
  let a = unpack_bool x and b = unpack_bool y in EvtBool(op a b)

let bool_unop x (op: bool -> bool) =
  let a = unpack_bool x in EvtBool(op a)

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
  | Plus   (x, y) ->   int_binop   (ieval x, ieval y)  (+)
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
  (* Special Primitives that are eval-recursive *)
  (* Map a function over an iterable structure *)
  | Apply (Symbol "map", args) ->
    let (f, s) = (match args with
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
  | Apply (Symbol "foldl", args) ->
    let (f, ac, s) = (match args with
      | [f; ac; s] -> (f, ac, s)
      | _ -> raise WrongPrimitiveArgs) in
    let ef = ieval f and es = ieval s and a = ieval ac in
    typecheck ef "fun";
    (match es with
      | EvtList x -> (List.fold_left
      (fun acc x -> applyfun ef [AlreadyEvaluated acc; AlreadyEvaluated x] n vb) a x)
      | EvtDict d ->
        let (_, values) = unzip d in
        (List.fold_left (fun acc x -> applyfun ef [AlreadyEvaluated acc; AlreadyEvaluated x] n vb) a values)
      | _ -> failwith "Value is not iterable")
  | Apply (Symbol "filter", args) ->
    let (p, s) = (match args with
      | [p; s] -> (ieval p, ieval s)
      | _ -> raise WrongPrimitiveArgs) in
    typecheck p "fun";
    (match s with
      | EvtList x ->
        EvtList(List.filter
        (fun x -> applyfun p [AlreadyEvaluated x] n vb = EvtBool true) x)
      | EvtDict d ->
        EvtDict(List.filter (fun (_,v) ->
          applyfun p [AlreadyEvaluated v] n vb = EvtBool true) d)
      | _ -> failwith "Value is not iterable")
  (* Function Application *)
  | Apply(f, args) ->
    let applyaux f args =
      let closure = ieval f in
      let evaluated_params = List.map (fun x -> AlreadyEvaluated (ieval x)) args in
      applyfun closure evaluated_params n vb in
    (match f with
      | Symbol s -> if key_exist s Primitives.table
        then (get_key_val s Primitives.table) (List.map ieval args)
        else applyaux f args
      | _ -> applyaux f args)
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
    | Closure(params, body, decenv) -> (* Use static scoping *)
      if (List.compare_lengths params evaluated_params) > 0 then (* curry *)
        let applied_env = bindlist decenv (take p_length params) evaluated_params in
        Closure((drop p_length params), body, applied_env)
      else  (* apply the function *)
        let application_env = bindlist decenv params evaluated_params in
        eval body application_env n vb
    (* Apply a recursive function *)
    | RecClosure(name, params, body, decenv) ->
      let rec_env = (bind decenv name (AlreadyEvaluated closure)) in
      if (List.compare_lengths params evaluated_params) > 0 then (* curry *)
        let applied_env = bindlist rec_env (take p_length params) evaluated_params in
        RecClosure(name, (drop p_length params), body, applied_env)
      else  (* apply the function *)
        let application_env = bindlist rec_env params evaluated_params in
        eval body application_env n vb
    | _ -> raise (TypeError "Cannot apply a non functional value")