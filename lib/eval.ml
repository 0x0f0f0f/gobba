open Types
open Interface
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
let rec eval (e: expr) (opts: evalopts) : evt =
  let opts = {opts with stack = push_stack opts.stack e} in
  (* Partially apply eval to the current stackframe, verbosity and environment *)
  if opts.verbosity >= 2 then print_message ~color:T.Blue ~loc:(Nowhere)
    "Reduction at depth" "%d\nExpression:\n%s" (depth_of_stack opts.stack) (show_expr e)
  else ();
  let evaluated = (match e with
  | Unit -> EvtUnit
  | Integer n -> EvtInt n
  | Boolean b -> EvtBool b
  | String s -> EvtString s
  | Symbol x -> lookup x opts
  | List x -> EvtList (List.map (fun x -> eval x opts) x)
  | Cons (x, xs) ->
    let ls = unpack_list (eval xs opts) in
    (match ls with
      | [] -> EvtList([(eval x opts)])
      | lss -> EvtList((eval x opts)::lss))
  (* Dictionaries and operations *)
  | Dict(l) ->
    let el = uniqueorfail (List.map
    (fun (x,y) -> isvalidkey (eval x opts, eval y opts )) l) in
    EvtDict el
  | Plus   (x, y) ->  int_binop   (eval x opts, eval y opts)  (+)
  | Sub   (x, y) ->   int_binop   (eval x opts, eval y opts)  (-)
  | Mult  (x, y) ->   int_binop   (eval x opts, eval y opts)  ( * )
  | And   (x, y) ->   bool_binop  (eval x opts, eval y opts)  (&&)
  | Or    (x, y) ->   bool_binop  (eval x opts, eval y opts)  (||)
  | Not   x      ->   bool_unop   (eval x opts)           (not)
  | Eq    (x, y) ->   EvtBool(compare_evt (eval x opts) (eval y opts) = 0)
  | Gt    (x, y) ->   EvtBool(compare_evt (eval x opts) (eval y opts) > 0)
  | Lt    (x, y) ->   EvtBool(compare_evt (eval x opts) (eval y opts) < 0)
  | Ge    (x, y) ->   EvtBool(compare_evt (eval x opts) (eval y opts) >= 0)
  | Le    (x, y) ->   EvtBool(compare_evt (eval x opts) (eval y opts) <= 0)
  | IfThenElse (guard, first, alt) ->
    let g = unpack_bool (eval guard opts) in
    if g then eval first opts else eval alt opts
  | Let (assignments, body) ->
    let evaluated_assignments = List.map
      (fun (_, value) -> AlreadyEvaluated (eval value opts)) assignments
    and identifiers = fstl assignments in
    let new_env = bindlist opts.env identifiers evaluated_assignments in
    eval body {opts with env = new_env}
  | Letlazy (assignments, body) ->
    let identifiers = fstl assignments in
    let new_env = bindlist opts.env identifiers
      (List.map (fun (_, value) -> LazyExpression value) assignments) in
    eval body {opts with env = new_env}
  | Letrec (ident, value, body) ->
    (match value with
      | Lambda (params, fbody) ->
        let rec_env = (bind opts.env ident
          (AlreadyEvaluated (RecClosure(ident, params, fbody, opts.env))))
        in eval body {opts with env = rec_env}
      | _ -> raise (TypeError "Cannot define recursion on non-functional values"))
  | Letreclazy (ident, value, body) ->
    (match value with
      | Lambda (_, _) ->
        let rec_env = (bind opts.env ident (LazyExpression value))
        in eval body {opts with env = rec_env}
      | _ -> raise (TypeError "Cannot define recursion on non-functional values"))
  | Lambda (params,body) -> Closure(params, body, opts.env)
  (* Special Primitives that are eval-recursive *)
  (* Map a function over an iterable structure *)
  | Apply (Symbol "map", args) ->
    let (f, s) = (match args with
      | [f; s] -> (f, s)
      | _ -> raise WrongPrimitiveArgs) in
    let ef = eval f opts and es = eval s opts in
    typecheck ef "fun";
    (match es with
      | EvtList x ->
        EvtList(List.map (fun x -> applyfun ef [AlreadyEvaluated x] opts) x)
      | EvtDict d ->
        let (keys, values) = unzip d in
        EvtDict(zip keys (List.map (fun x -> applyfun ef [AlreadyEvaluated x] opts) values))
      | _ -> failwith "Value is not iterable")
  | Apply (Symbol "map2", args) ->
    let (f, s1, s2) = (match args with
      | [f; s1; s2] -> (f, s1, s2)
      | _ -> raise WrongPrimitiveArgs) in
    let ef = eval f opts and es1 = eval s1 opts and es2 = eval s2 opts in
    typecheck ef "fun";
    (match es1 with
      | EvtList x ->
        let y = unpack_list es2 in
        EvtList(List.map2 (fun a b -> applyfun ef [AlreadyEvaluated a;
        AlreadyEvaluated b] opts) x y)
      | _ -> failwith "Value is not iterable")
  | Apply (Symbol "foldl", args) ->
    let (f, ac, s) = (match args with
      | [f; ac; s] -> (f, ac, s)
      | _ -> raise WrongPrimitiveArgs) in
    let ef = eval f opts and es = eval s opts and a = eval ac opts in
    typecheck ef "fun";
    (match es with
      | EvtList x -> (List.fold_left
      (fun acc x -> applyfun ef [AlreadyEvaluated acc; AlreadyEvaluated x] opts) a x)
      | EvtDict d ->
        let (_, values) = unzip d in
        (List.fold_left (fun acc x -> applyfun ef [AlreadyEvaluated acc;
        AlreadyEvaluated x] opts) a values)
      | _ -> failwith "Value is not iterable")
  | Apply (Symbol "filter", args) ->
    let (p, s) = (match args with
      | [p; s] -> (eval p opts, eval s opts)
      | _ -> raise WrongPrimitiveArgs) in
    typecheck p "fun";
    (match s with
      | EvtList x ->
        EvtList(List.filter
        (fun x -> applyfun p [AlreadyEvaluated x] opts = EvtBool true) x)
      | EvtDict d ->
        EvtDict(List.filter (fun (_,v) ->
          applyfun p [AlreadyEvaluated v] opts = EvtBool true) d)
      | _ -> failwith "Value is not iterable")
  (* Function Application *)
  | Apply(f, expr_args) ->
    let closure = eval f opts in
    let args = List.map (fun x -> AlreadyEvaluated (eval x opts)) expr_args in
    applyfun closure args opts
  (* Eval a sequence of expressions but return the last *)
  | Sequence(exprl) ->
    let rec unroll el = (match el with
    | [] -> failwith "fatal: empty command sequence"
    | x::[] -> eval x opts
    | x::xs -> (let _ = eval x opts in unroll xs)) in unroll exprl
  (* Pipe two functions together, creating a new function
     That uses the first functions's result as the second's first argument *)
  | Pipe(e1, e2) ->
    (* Convert a list of identifiers to a list of symbols *)
    let syml l = List.map (fun x -> Symbol x) l in
    let f1 = eval e1 opts and f2 = eval e2 opts in
    typecheck f2 "fun";
    let (_, params1, _, _) = unpack_anyfun f1 in
    Closure(params1, Apply(e2, [Apply(e1, syml params1)]), opts.env))
  in
  if opts.verbosity >= 2 then print_message ~color:T.Cyan ~loc:(Nowhere)
    "Evaluates to at depth" "%d\n%s\n" (depth_of_stack opts.stack) (show_evt evaluated)
  else ();
  evaluated;
(* Search for a value in the primitives table and environment *)
and lookup (ident: ide) (opts: evalopts) : evt =
  if key_exist ident Primitives.table
  then
    let ( _, numargs) = (get_key_val ident Primitives.table) in
    PrimitiveAbstraction (ident, numargs, opts.env)
  else lookup_env ident opts
(* Search for a value in an environment *)
and lookup_env (ident: ide) (opts: evalopts) : evt =
  if ident = "" then failwith "invalid identifier" else
  match opts.env with
  | [] -> raise (UnboundVariable ident)
  | (i, LazyExpression e) :: env_rest -> if ident = i then eval e opts
    else lookup_env ident {opts with env = env_rest}
  | (i, AlreadyEvaluated e) :: env_rest -> if ident = i then e else
    lookup_env ident {opts with env = env_rest}
and applyfun (closure: evt) (args: type_wrapper list) (opts: evalopts) : evt =
    (* Evaluate the arguments and unpack the evt encapsuled in them *)
    let args = List.map (fun x ->
      match x with
        | AlreadyEvaluated _ -> x
        | LazyExpression y -> AlreadyEvaluated (eval y opts)) args
    in let evtargs = List.map (fun x -> match x with
        | AlreadyEvaluated y -> y
        | LazyExpression _ -> failwith "FATAL ERROR: this should have never happened") args in
    let p_length = List.length args in
    (match closure with
      | Closure(params, body, decenv) -> (* Use static scoping *)
        if (List.compare_lengths params args) > 0 then (* curry *)
          let applied_env = bindlist decenv (take p_length params) args in
          Closure((drop p_length params), body, applied_env)
        else  (* apply the function *)
          let application_env = bindlist decenv params args in
          eval body {opts with env = application_env}
      (* Apply a recursive function *)
      | RecClosure(name, params, body, decenv) ->
        let rec_env = (bind decenv name (AlreadyEvaluated closure)) in
        if (List.compare_lengths params args) > 0 then (* curry *)
          let applied_env = bindlist rec_env (take p_length params) args in
          RecClosure(name, (drop p_length params), body, applied_env)
        else  (* apply the function *)
          let application_env = bindlist rec_env params args in
          eval body {opts with env = application_env}
      | PrimitiveAbstraction(name, numargs, decenv) ->
        if (numargs > p_length) then (* curry *)
          let primargs = generate_prim_params (numargs) in
          let symprimargs = List.map (fun x -> Symbol x) primargs in
          let missing_args = drop p_length primargs
          and ihavethose_args = take p_length primargs in
          let app_env = bindlist decenv ihavethose_args args in
          Closure(missing_args, Apply(Symbol name, symprimargs), app_env)
        else
          (* Apply the primitive *)
          let (prim, _) = get_key_val name Primitives.table in
          prim evtargs
      | _ -> raise (TypeError "Cannot apply a non functional value"))