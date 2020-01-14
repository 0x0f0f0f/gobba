open Types
open Util
open Typecheck
module T = ANSITerminal

(** Boolean Primitives *)

let bool_binop (x, y) (op : bool -> bool -> bool) =
  let a = unpack_bool x and b = unpack_bool y in
  EvtBool (op a b)

let bool_unop x (op : bool -> bool) =
  let a = unpack_bool x in
  EvtBool (op a)

let uniqueorfail l =
  if Dict.dup_exists l then iraise (DictError "Duplicate key in dictionary")
  else l

let checkstrictpurity current newp = if isstrictlypure current && isimpure newp then
  iraise (PurityError "Cannot enter an impure context from a strictly pure one")
  else ()

let checkpurity current newp = if ispure current && isimpure newp then
  iraise (PurityError "Cannot enter an impure context from a strictly pure one")
  else ()

(** Evaluate an expression in an environment *)
let rec eval ?knownpurity:(knownpurity=None) (e : expr) (state : evalstate) : evt =
  let state = { state with stack = push_stack state.stack e } in
  let epurity = match knownpurity with
    | Some p -> p
    | None -> infer_purity e Primitives.table [] state.env in
  if state.verbosity >= 2 then
    print_message ~color:T.Blue ~loc:Nowhere "Reduction at depth"
      "%d\n%s Expression:\n%s"
      (depth_of_stack state.stack) (show_puret epurity)
      (show_expr e)
  else ();
  checkstrictpurity state.purity epurity;
  let evaluated =
    match e with
    | Unit -> EvtUnit
    | Purity (n, ee) -> eval ee { state with purity = n }
    | NumInt n -> EvtInt n
    | NumFloat n -> EvtFloat n
    | NumComplex n -> EvtComplex n
    | Boolean b -> EvtBool b
    | String s -> EvtString s
    | Symbol x -> lookup x state
    | List x -> EvtList (List.map (fun x -> eval x state) x)
    | Cons (x, xs) -> (
        let ls = unpack_list (eval xs state) in
        match ls with
        | [] -> EvtList [ eval x state ]
        | lss -> EvtList (eval x state :: lss) )
    | ConcatLists(e1, e2) -> EvtList ((unpack_list (eval e1 state)) @ (unpack_list (eval e2 state)))
    | ConcatStrings(e1, e2) -> EvtString ((unpack_string (eval e1 state)) ^ (unpack_string (eval e2 state)))
    (* Dictionaries and operations *)
    | Dict l ->
      let el =
        uniqueorfail
          (List.map (fun (x, y) -> (x, eval y state)) l)
      in
      EvtDict el
    | Plus (x, y) ->  Numericalp.add [(eval x state); (eval y state)]
    | Sub (x, y) ->   Numericalp.sub [(eval x state); (eval y state)]
    | Div (x, y) ->   Numericalp.div [(eval x state); (eval y state)]
    | Mult (x, y) ->  Numericalp.mult [(eval x state); (eval y state)]
    | And (x, y) -> bool_binop (eval x state, eval y state) ( && )
    | Or (x, y) -> bool_binop (eval x state, eval y state) ( || )
    | Not x -> bool_unop (eval x state) not
    | Eq (x, y) -> EvtBool (compare_evt (eval x state) (eval y state) = 0)
    | Gt (x, y) -> EvtBool (compare_evt (eval x state) (eval y state) > 0)
    | Lt (x, y) -> EvtBool (compare_evt (eval x state) (eval y state) < 0)
    | Ge (x, y) -> EvtBool (compare_evt (eval x state) (eval y state) >= 0)
    | Le (x, y) -> EvtBool (compare_evt (eval x state) (eval y state) <= 0)
    | IfThenElse (guard, first, alt) ->
      let g = unpack_bool (eval guard state) in
      if g then eval first state else eval alt state
    | Let (assignments, body) ->
      let evaluated_assignments =
        List.map
          (fun (_, value) -> AlreadyEvaluated (eval value state) )
          assignments
      and identifiers = fstl assignments in
      let new_env =
        Dict.insertmany state.env identifiers evaluated_assignments
      in
      eval ~knownpurity:(Some epurity) body { state with env = new_env }
    | Letlazy (assignments, body) ->
      let identifiers = fstl assignments in
      let new_env =
        Dict.insertmany state.env identifiers
          (List.map (fun (_, value) -> LazyExpression value) assignments)
      in
      eval ~knownpurity:(Some epurity) body { state with env = new_env }
    | Letrec (ident, value, body) -> (
        match value with
        | Lambda (params, fbody) ->
          let rec_env =
            Dict.insert state.env ident
              (AlreadyEvaluated (RecClosure (ident, params, fbody, state.env, epurity)))
          in
          eval ~knownpurity:(Some epurity) body { state with env = rec_env }
        | _ ->
          traise "Cannot define recursion on non-functional values"
      )
    | Letreclazy (ident, value, body) -> (
        match value with
        | Lambda (_, _) ->
          let rec_env = Dict.insert state.env ident (LazyExpression value) in
          eval  ~knownpurity:(Some epurity) body { state with env = rec_env }
        | _ ->
          traise "Cannot define recursion on non-functional values"
      )
    | Lambda (param, body) ->
      Closure (param, body, state.env, epurity)
    | Compose (f2, f1) ->
      let ef1 = eval f1 state and ef2 = eval f2 state in
      stcheck (typeof ef1) (TLambda Uncertain); stcheck (typeof ef2) (TLambda Uncertain);
      let params1 = findevtparams ef1 in
      let appl1 = apply_from_exprlist (symbols_from_strings params1) f1 in
      eval (lambda_from_paramlist params1 (Apply (f2, appl1))) state
    (* Function Application *)
    | Apply (f, arg) ->
      let closure = eval f state in
      let earg = (AlreadyEvaluated ((eval arg state))) in
      applyfun closure earg state
    | ApplyPrimitive ((name, numparams, purity), args) ->
      if List.length args != numparams then (iraise (Fatal "Primitive Application Error"))
      else if ispure state.purity && isimpure purity then
        iraise
          (PurityError ("Tried to apply an impure primitive in a pure block: " ^ name))
      else
      let eargs = List.map (fun x -> eval x state) args in
      let prim = get_primitive_function (Dict.get name Primitives.table) in
      prim eargs
    (* Eval a sequence of expressions but return the last *)
    | Sequence exprl ->
      let rec unroll el =
        match el with
        | [] -> EvtUnit
        | [ x ] -> eval x state
        | x :: xs ->
          let _ = eval x state in
          unroll xs
      in
      unroll exprl
  in
  if state.verbosity >= 2 then
    print_message ~color:T.Cyan ~loc:Nowhere "Evaluates to at depth" "%d\n%s\n"
      (depth_of_stack state.stack)
      (show_evt evaluated)
  else ();
  evaluated

(* Search for a value in the primitives table and environment *)
and lookup (ident : ide) (state : evalstate) : evt =
  if Dict.exists ident Primitives.table then
    let _, numparams, purity = get_primitive_info (Dict.get ident Primitives.table) in
    (* Generate a closure abstraction from a primitive *)
    let primargs = generate_prim_params numparams in
    let symprimargs = symbols_from_strings primargs in
    let lambdas = lambda_from_paramlist primargs (ApplyPrimitive((ident, numparams, purity), symprimargs)) in
    eval lambdas state
  else if Dict.exists ident Primitives.stdlib_table then
    (Dict.get ident Primitives.stdlib_table)
  else lookup_env ident state

(* Search for a value in an environment *)
and lookup_env (ident : ide) (state : evalstate) : evt =
  if ident = "" then failwith "invalid identifier"
  else
    match state.env with
    | [] -> iraise (UnboundVariable ident)
    | (i, LazyExpression e) :: env_rest ->
      if ident = i then eval e state
      else lookup_env ident { state with env = env_rest }
    | (i, AlreadyEvaluated e) :: env_rest ->
      if ident = i then e else lookup_env ident { state with env = env_rest }

and applyfun (closure : evt) (arg : type_wrapper) (state : evalstate) : evt =
  (* Evaluate the argument and unpack the evt encapsuled in them *)
  match closure with
  | Closure (param, body, decenv, cpurity) ->
      (* apply the function *)
      let application_env = Dict.insert decenv param arg in
      eval ~knownpurity:(Some cpurity) body { state with env = application_env }
  (* Apply a recursive function *)
  | RecClosure (name, param, body, decenv, cpurity) ->
    let rec_env = Dict.insert decenv name (AlreadyEvaluated closure) in
    let application_env = Dict.insert rec_env param arg in
    eval ~knownpurity:(Some cpurity) body { state with env = application_env }
    (* Generate a closure abstraction from a primitive *)
  | _ -> traise "Cannot apply a non functional value"
