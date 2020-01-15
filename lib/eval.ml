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
    | Cons (x, xs) -> (
        let ls = unpack_list (eval xs state) in
        match ls with
        | [] -> EvtList [ eval x state ]
        | lss -> EvtList (eval x state :: lss) )
    | Concat(e1, e2) ->
      let ev1 = eval e1 state and ev2 = eval e2 state in
      let t1 = typeof ev1 and t2 = typeof ev2 in
      (match (t1, t2) with
        | TString, TString -> EvtString ((unpack_string ev1) ^ (unpack_string ev2))
        | TList, TList -> EvtList ((unpack_list ev1) @ (unpack_list ev2))
        | _ -> iraises (TypeError (Printf.sprintf "Cannot concatenate a two values of type %s and %s"
          (show_tinfo t1) (show_tinfo t2))) state.stack )
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
    | Let (assignments, body) -> eval body (eval_assignment_list assignments state)
    | Lambda (param, body) ->
      Closure (None, param, body, state.env)
    | Compose (f2, f1) ->
      let ef1 = eval f1 state and ef2 = eval f2 state in
      stcheck (typeof ef1) TLambda; stcheck (typeof ef2) TLambda;
      let params1 = findevtparams ef1 in
      let appl1 = apply_from_exprlist (symbols_from_strings params1) f1 in
      eval (lambda_from_paramlist params1 (Apply (f2, appl1))) state
    (* Function Application *)
    | Apply (f, arg) ->
      let closure = eval f state in
      let earg = (AlreadyEvaluated ((eval arg state))) in
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

(* Search for a value in the primitives table and environment *)
and lookup (ident : ide) (state : evalstate) : evt =
  match (Dict.get ident Primitives.table) with
    | None -> (match (Dict.get ident state.env) with
      | None -> iraises (UnboundVariable ident) state.stack
      | Some (LazyExpression e) -> eval e state
      | Some (AlreadyEvaluated e) -> e)
    | Some (LazyExpression e) -> eval e state
    | Some (AlreadyEvaluated e) -> e

and applyfun (closure : evt) (arg : type_wrapper) (state : evalstate) : evt =
  (* Evaluate the argument and unpack the evt encapsuled in them *)
  match closure with
  | Closure (name, param, body, decenv) ->
    (* Create a recursion environment if the function is recursive *)
    let self_env = (match name with
        | None -> decenv
        | Some x -> Dict.insert decenv x (AlreadyEvaluated closure)) in
    let appl_env = Dict.insert self_env param arg in
    eval body { state with env = appl_env }
  | _ -> traise "Cannot apply a non functional value"

and eval_assignment state (islazy, name, value) : evalstate =
  let nval =
    if islazy then  LazyExpression value else
    (match value with
    | Lambda(param, fbody) ->
        let rec_env = Dict.insert state.env name (AlreadyEvaluated (Closure (Some name, param, fbody, state.env))) in
        (AlreadyEvaluated (eval value { state with env = rec_env }))
    | _ -> (AlreadyEvaluated (eval value state))) in
    { state with env = (Dict.insert state.env name nval) }

and eval_assignment_list assignment_list state : evalstate =
  match assignment_list with
  | [] -> state
  | (islazy, name, value)::xs ->
    let newstate = eval_assignment state (islazy, name, value) in
    (eval_assignment_list xs newstate)

and eval_command command state dirscope =
  if state.verbosity >= 1 then print_message ~loc:(Nowhere) ~color:T.Yellow
      "AST equivalent" (Printf.sprintf "\n%s"
      (show_command command)) else ();
  match command with
  | Directive dir -> eval_directive dir state dirscope
  | Expr e ->
    (* Infer the expression purity and evaluate if appropriate to the current state *)
    let exprpurity = Puritycheck.infer e state in
    if isstrictlypure state.purity && isimpure exprpurity then
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

and eval_command_list cmdlst state dirscope = match cmdlst with
  | x::xs ->
    let _, newstate = eval_command x state dirscope in
    eval_command_list xs newstate dirscope;
  | [] -> (EvtUnit, state)

and eval_directive dir state dirscope =
  match dir with
    | Dumppurityenv -> Printf.eprintf "<purity_env>: %s\n%!" (show_purityenv_type state.purityenv); (EvtUnit, state)
    | Includefileasmodule (f, m) ->
      let modulename = (match m with
        | Some m -> m
        | None -> Filename.remove_extension f |> Filename.basename |> String.capitalize_ascii) in
      let file_in_scope = if not (Filename.is_relative f) then f else
      Filename.concat (dirscope) f in
      let _, resulting_state = eval_command_list (read_file (Parser.file Lexer.token) file_in_scope)
        { state with env = []; purityenv = [] } dirscope in
      let newmodule =
        List.filter (fun (_,v) -> match v with AlreadyEvaluated _ -> true | _ -> false) resulting_state.env
        |> List.map (fun (k, v) -> match v with AlreadyEvaluated x -> (k,x) | _ -> failwith "should never fail")
        |> fun ls -> EvtDict ls in
      (EvtUnit, { state with env = (Dict.insert state.env modulename (AlreadyEvaluated newmodule) ) })

    | Includefile f ->
      let file_in_scope = if not (Filename.is_relative f) then f else
      Filename.concat (dirscope) f in
      (* Eval the file contents *)
      eval_command_list (read_file (Parser.file Lexer.token) file_in_scope) state dirscope
    | Setpurity p ->
      if state.verbosity >= 1 then
      Printf.eprintf "%s%!" (show_puret state.purity) else ();
      (EvtUnit, { state with purity = p })
    | Setverbose v -> (EvtUnit, { state with verbosity = v})