open Types
open Errors
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

(** Evaluate an expression in an environment *)
let rec eval (e : expr) (state : evalstate) : evt =
  let state = { state with stack = push_stack state.stack e } in

  if state.verbosity >= 2 then
    print_message ~color:T.Blue ~loc:Nowhere "Reduction at depth"
      (Printf.sprintf "%d\nExpression:\n%s" (depth_of_stack state.stack) (show_expr e))
  else ();
  let evaluated =
    match e with
    | Unit -> EvtUnit
    | Purity (allowed, ee) ->
      eval ee { state with purity = allowed }
    | NumInt n -> EvtInt n
    | NumFloat n -> EvtFloat n
    | NumComplex n -> EvtComplex n
    | Boolean b -> EvtBool b
    | String s -> EvtString s
    | Symbol x -> lookup x state
    | List x -> EvtList (List.map (fun x -> eval x state) x)
    | Binop (kind, e1, e2) -> eval_binop kind e1 e2 state
    (* Dictionaries and operations *)
    | Dict l ->
      let el = uniqueorfail (List.map (eval_assignment state) l) in
      EvtDict el
    | Not x -> bool_unop (eval x state) not
    | IfThenElse (guard, first, alt) ->
      let g = unpack_bool (eval guard state) in
      if g then eval first state else eval alt state
    | Let (assignments, body) -> eval body (eval_assignment_list assignments state)
    | Lambda (param, body) ->
      Closure (None, param, body, state.env)
    (* Function Application *)
    | Apply (f, arg) ->
      let closure = eval f state in
      let earg = eval arg state in
      applyfun closure earg state
    | ApplyPrimitive ((name, _, _), args) ->
      let eargs = List.map (fun x -> eval x state) args in
      let prim = get_primitive_function (match (Dict.get name Primitives.ocaml_table) with
          | None -> iraise (Fatal "Unbound primitive. This should never happen")
          | Some p -> p) in
      (try prim eargs with InternalError (loc, err, _) -> raise (InternalError(loc, err, state.stack)))
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
    print_message ~color:T.Cyan ~loc:Nowhere "Evaluates to at depth" (Printf.sprintf "%d\n%s\n"
                                                                        (depth_of_stack state.stack)
                                                                        (show_evt evaluated))
  else ();
  evaluated

and eval_binop (k: binop) (x: expr) (y: expr) state =
  match k with
  | Getkey ->
    let key = (match y with Symbol z -> z | _ -> iraise (Fatal "Dictionary access"))
    and ed = unpack_dict (eval x state) in
    (match Dict.get key ed with
     | None -> iraise (DictError "key not found")
     | Some (LazyExpression z) -> eval z state
     | Some z -> z)
  | Cons ->
    let ls = unpack_list (eval y state) in
    (match ls with
     | [] -> EvtList [ eval x state ]
     | lss -> EvtList (eval x state :: lss))
  | Concat ->
    let ev1 = eval x state and ev2 = eval y state in
    let t1 = typeof ev1 and t2 = typeof ev2 in
    (match (t1, t2) with
     | TString, TString -> EvtString ((unpack_string ev1) ^ (unpack_string ev2))
     | TList, TList -> EvtList ((unpack_list ev1) @ (unpack_list ev2))
     | _ -> iraises (TypeError (Printf.sprintf "Cannot concatenate a two values of type %s and %s"
                                  (show_tinfo t1) (show_tinfo t2))) state.stack )
  | Compose->
    let ef1 = eval y state and ef2 = eval x state in
    stcheck (typeof ef1) TLambda; stcheck (typeof ef2) TLambda;
    let params1 = findevtparams ef1 in
    let appl1 = apply_from_exprlist (symbols_from_strings params1) y in
    eval (lambda_from_paramlist params1 (Apply (x, appl1))) state

  | Plus  ->  Numericalp.add [(eval x state); (eval y state)]
  | Sub  ->   Numericalp.sub [(eval x state); (eval y state)]
  | Div  ->   Numericalp.div [(eval x state); (eval y state)]
  | Mult  ->  Numericalp.mult [(eval x state); (eval y state)]
  | And  -> bool_binop (eval x state, eval y state) ( && )
  | Or -> bool_binop (eval x state, eval y state) ( || )
  | Eq -> EvtBool (compare_evt (eval x state) (eval y state) = 0)
  | Gt -> EvtBool (compare_evt (eval x state) (eval y state) > 0)
  | Lt -> EvtBool (compare_evt (eval x state) (eval y state) < 0)
  | Ge -> EvtBool (compare_evt (eval x state) (eval y state) >= 0)
  | Le -> EvtBool (compare_evt (eval x state) (eval y state) <= 0)


(* Search for a value in the primitives table and environment *)
and lookup (ident : ide) (state : evalstate) : evt =
  match (Dict.get ident Primitives.table) with
  | None -> (match (Dict.get ident state.env) with
      | None -> iraises (UnboundVariable ident) state.stack
      | Some (LazyExpression e) -> eval e state
      | Some e -> e)
  | Some (LazyExpression e) -> eval e state
  | Some e -> e

and applyfun (closure : evt) (arg : evt) (state : evalstate) : evt =
  (* Evaluate the argument and unpack the evt encapsuled in them *)
  match closure with
  | Closure (name, param, body, decenv) ->
    (* Create a recursion environment if the function is recursive *)
    let self_env = (match name with
        | None -> decenv
        | Some x -> Dict.insert decenv x closure) in
    let appl_env = Dict.insert self_env param arg in
    eval body { state with env = appl_env }
  | _ -> traise "Cannot apply a non functional value"

and eval_assignment state (islazy,name,value)  =
  if islazy then (name, LazyExpression value) else
    (match value with
    | Lambda(param, fbody) ->
        let rec_env = Dict.insert state.env name (Closure (Some name, param, fbody, state.env)) in
        name, eval value { state with env = rec_env }
    | _ -> name, eval value state)

and eval_assignment_list assignment_list state : evalstate =
  match assignment_list with
  | [] -> state
  | (islazy, name, value)::xs ->
    let _, nval = eval_assignment state (islazy, name, value) in
    (eval_assignment_list xs { state with env = (Dict.insert state.env name nval) })

and eval_command command state dirscope =
  if state.verbosity >= 1 then print_message ~loc:(Nowhere) ~color:T.Yellow
      "AST equivalent" (Printf.sprintf "\n%s"
                          (show_command command)) else ();
  match command with
  | Directive dir -> eval_directive dir state dirscope
  | Expr e ->
    (* Infer the expression purity and evaluate if appropriate to the current state *)
    let exprpurity = Puritycheck.infer e state in
    if (state.purity = Pure || state.purity = Numerical) && exprpurity = Impure then
      iraises (PurityError ("This expression contains a " ^ (show_puret exprpurity) ^
                            " expression but it is in " ^ (show_puret state.purity) ^ " state!")) state.stack else ();
    if state.verbosity >= 1 then Printf.eprintf "Has purity: %s\n%!" (show_puret exprpurity) else ();
    (* Normalize the expression *)
    let optimized_ast = Optimizer.iterate_optimizer e in
    (* If the expression is NOT already in normal state, print the optimized one if verbosity is enough *)
    if optimized_ast = e then () else
    if state.verbosity >= 1 then print_message ~loc:(Nowhere) ~color:T.Yellow "After AST optimization"
        (Printf.sprintf "\n%s" (show_expr optimized_ast)) else ();
    (* Evaluate the expression *)
    let evaluated = eval optimized_ast state in
    (* Print it in its raw form if verbosity is enabled *)
    if state.verbosity >= 1 then print_message ~color:T.Green ~loc:(Nowhere) "Result"
        (Printf.sprintf "\t%s" (show_evt evaluated)) else ();
    (* Print the fancy result if state.printresult is true *)
    if state.printresult then
      Printf.eprintf "result: %s - %s\n%!"
        (show_unpacked_evt evaluated)
        (show_tinfo (Typecheck.typeof evaluated))
    else ();
    (evaluated, state)
  | Def dl ->
    let (islazyl, idel, vall) = unzip3 dl in
    (* Infer the values purity and evaluate if appropriate to the current state *)
    let new_purity_state = Puritycheck.infer_assignment_list dl state in
    let ovall = (List.map (Optimizer.iterate_optimizer) vall) in
    let odl = zip3 islazyl idel ovall in
    (* Print the definitions if verbosity is enough and they were optimized *)
    if ovall = vall then () else
    if state.verbosity >= 1 then print_message ~loc:(Nowhere) ~color:T.Yellow "After AST optimization"
        (Printf.sprintf "\n%s" (show_command (Def odl))) else ();
    let newstate = eval_assignment_list odl new_purity_state in
    (EvtUnit, newstate )

and eval_command_list cmdlst state dirscope =
  let mstate = ref state in
  List.iter (fun x -> mstate := snd (eval_command x !mstate dirscope)) cmdlst;
  (EvtUnit, !mstate)

and eval_directive dir state dirscope =
  match dir with
  | Dumpenv -> Printf.eprintf "<env>: %s\n%!" (show_env_type state.env); (EvtUnit, state)
  | Dumppurityenv -> Printf.eprintf "<purity_env>: %s\n%!" (show_purityenv_type state.purityenv); (EvtUnit, state)
  | Includefileasmodule (f, m) ->
    let modulename = (match m with
        | Some m -> m
        | None -> Filename.remove_extension f |> Filename.basename |> String.capitalize_ascii) in
    let file_in_scope = if not (Filename.is_relative f) then f else
        Filename.concat (dirscope) f in
    let _, resulting_state = eval_command_list (Parsedriver.read_file file_in_scope)
        { state with env = []; purityenv = [] } dirscope in
    let newmodule = EvtDict resulting_state.env in
    (EvtUnit, { state with env = (Dict.insert state.env modulename newmodule ) })
  | Includefile f ->
    let file_in_scope = if not (Filename.is_relative f) then f else
        Filename.concat (dirscope) f in
    (* Eval the file contents *)
    eval_command_list (Parsedriver.read_file file_in_scope) state dirscope
  | Setpurity p ->
    if state.verbosity >= 1 then
      Printf.eprintf "%s%!" (show_puret state.purity) else ();
    (EvtUnit, { state with purity = p })
  | Setverbose v -> (EvtUnit, { state with verbosity = v})