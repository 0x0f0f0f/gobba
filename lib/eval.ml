open Types
open Interface
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
  if Dict.dup_exists l then raise (DictError "Duplicate key in dictionary")
  else l

(** Evaluate an expression in an environment *)
let rec eval (e : expr) (state : evalstate) : evt =
  let state = { state with stack = push_stack state.stack e } in
  (* Partially apply eval to the current stackframe, verbosity and environment *)
  if state.verbosity >= 2 then
    print_message ~color:T.Blue ~loc:Nowhere "Reduction at depth"
      "%d\nExpression:\n%s"
      (depth_of_stack state.stack)
      (show_expr e)
  else ();
  let evaluated =
    match e with
    | Unit -> EvtUnit
    | Purity (n, ee) ->
      if state.purity = Pure && n = Impure then
          raise (PurityError "Cannot enter an impure contest from a strictly pure one")
      else eval ee { state with purity = n }
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
          (List.map (fun (x, y) -> isvalidkey (eval x state, eval y state)) l)
      in
      EvtDict el
    | Plus (x, y) ->  Numerical.add [(eval x state); (eval y state)]
    | Sub (x, y) -> Numerical.sub [(eval x state); (eval y state)]
    | Div (x, y) -> Numerical.div [(eval x state); (eval y state)]
    | Mult (x, y) -> Numerical.mult [(eval x state); (eval y state)]
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
          (fun (_, value) -> AlreadyEvaluated (eval value state))
          assignments
      and identifiers = fstl assignments in
      let new_env =
        Dict.insertmany state.env identifiers evaluated_assignments
      in
      eval body { state with env = new_env }
    | Letlazy (assignments, body) ->
      let identifiers = fstl assignments in
      let new_env =
        Dict.insertmany state.env identifiers
          (List.map (fun (_, value) -> LazyExpression value) assignments)
      in
      eval body { state with env = new_env }
    | Letrec (ident, value, body) -> (
        match value with
        | Lambda (params, fbody) ->
          let rec_env =
            Dict.insert state.env ident
              (AlreadyEvaluated (RecClosure (ident, params, fbody, state.env)))
          in
          eval body { state with env = rec_env }
        | _ ->
          raise (TypeError "Cannot define recursion on non-functional values")
      )
    | Letreclazy (ident, value, body) -> (
        match value with
        | Lambda (_, _) ->
          let rec_env = Dict.insert state.env ident (LazyExpression value) in
          eval body { state with env = rec_env }
        | _ ->
          raise (TypeError "Cannot define recursion on non-functional values")
      )
    | Lambda (params, body) -> Closure (params, body, state.env)
    (* Function Application *)
    | Apply (f, expr_args) ->
      let closure = eval f state in
      let args =
        List.map (fun x -> AlreadyEvaluated (eval x state)) expr_args
      in
      applyfun closure args state
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
    (* Pipe two functions together, creating a new function
       That uses the first functions's result as the second's first argument *)
    | Pipe (e1, e2) ->
      (* Convert a list of identifiers to a list of symbols *)
      let syml l = List.map (fun x -> Symbol x) l in
      let f1 = eval e1 state and f2 = eval e2 state in
      stcheck (typeof f2) TLambda;
      let _, params1, _, _ = unpack_anyfun f1 in
      Closure (params1, Apply (e2, [ Apply (e1, syml params1) ]), state.env)
  in
  if state.verbosity >= 2 then
    print_message ~color:T.Cyan ~loc:Nowhere "Evaluates to at depth" "%d\n%s\n"
      (depth_of_stack state.stack)
      (show_evt evaluated)
  else ();
  evaluated

(* Search for a value in the primitives table and environment *)
and lookup (ident : ide) (state : evalstate) : evt =
  if Dict.exists ident Primitives.fulltable then
    let _, numargs = Dict.get ident Primitives.fulltable in
    let purity =
    if Dict.exists ident Primitives.impure_table then Impure else Pure in
    PrimitiveAbstraction (ident, numargs, state.env, purity)
  else lookup_env ident state

(* Search for a value in an environment *)
and lookup_env (ident : ide) (state : evalstate) : evt =
  if ident = "" then failwith "invalid identifier"
  else
    match state.env with
    | [] -> raise (UnboundVariable ident)
    | (i, LazyExpression e) :: env_rest ->
      if ident = i then eval e state
      else lookup_env ident { state with env = env_rest }
    | (i, AlreadyEvaluated e) :: env_rest ->
      if ident = i then e else lookup_env ident { state with env = env_rest }

and applyfun (closure : evt) (args : type_wrapper list) (state : evalstate) : evt =
  (* Evaluate the arguments and unpack the evt encapsuled in them *)
  let args =
    List.map
      (fun x ->
         match x with
         | AlreadyEvaluated _ -> x
         | LazyExpression y -> AlreadyEvaluated (eval y state))
      args
  in
  let evtargs =
    List.map
      (fun x ->
         match x with
         | AlreadyEvaluated y -> y
         | LazyExpression _ ->
           failwith "FATAL ERROR: this should have never happened")
      args
  in
  let p_length = List.length args in
  match closure with
  | Closure (params, body, decenv) ->
    (* Use static scoping *)
    if List.compare_lengths params args > 0 then
      (* curry *)
      let applied_env = Dict.insertmany decenv (take p_length params) args in
      Closure (drop p_length params, body, applied_env)
    else
      (* apply the function *)
      let application_env = Dict.insertmany decenv params args in
      eval body { state with env = application_env }
  (* Apply a recursive function *)
  | RecClosure (name, params, body, decenv) ->
    let rec_env = Dict.insert decenv name (AlreadyEvaluated closure) in
    if List.compare_lengths params args > 0 then
      (* curry *)
      let applied_env = Dict.insertmany rec_env (take p_length params) args in
      RecClosure (name, drop p_length params, body, applied_env)
    else
      (* apply the function *)
      let application_env = Dict.insertmany rec_env params args in
      eval body { state with env = application_env }
  | PrimitiveAbstraction (name, numargs, decenv, ispure) ->
    if numargs > p_length then
      (* curry *)
      let primargs = generate_prim_params numargs in
      let symprimargs = List.map (fun x -> Symbol x) primargs in
      let missing_args = drop p_length primargs
      and ihavethose_args = take p_length primargs in
      let app_env = Dict.insertmany decenv ihavethose_args args in
      Closure (missing_args, Apply (Symbol name, symprimargs), app_env)
    else
      (* Apply the primitive *)
    if state.purity = Pure || state.purity = Uncertain && ispure = Impure then
      raise
        (PurityError ("Tried to apply an impure primitive in a pure block: " ^ name))
    else
      let prim = (fun x -> (fst (Dict.get name Primitives.fulltable)) x applyfun state) in
      prim evtargs
  | _ -> raise (TypeError "Cannot apply a non functional value")
