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

(** Evaluate an expression in a state
  @param e The expression to evaluate
  @param state The current evaluation state (immutable) altered in recursive calls *)
let rec eval (e : expr) (state : evalstate) : evt =
  let state = { state with stack = Estack.push_stack state.stack e } in

  if state.verbosity >= 2 then
    print_message ~color:T.Blue ~loc:Nowhere "Reduction at depth"
      (Printf.sprintf "%d\nExpression:\n%s" (Estack.depth_of_stack state.stack) (show_expr e))
  else ();
  let evaluated =
    match e with
    (* Purity blocks call eval on the body with an altered purity in the state *)
    | SetPurity (allowed, ee) ->
      eval ee { state with purity = allowed }
    (* Simple types *)
    | Unit -> EvtUnit
    | NumInt n -> EvtInt n
    | NumFloat n -> EvtFloat n
    | NumComplex n -> EvtComplex n
    | Boolean b -> EvtBool b
    | Character c -> EvtChar c
    | String s -> EvtString s
    | Symbol x -> lookup x state
    (* Binary operations *)
    | Binop (kind, e1, e2) -> eval_binop kind e1 e2 state
    (* Lists and vectors *)
    | List x -> EvtList (List.map (fun x -> eval x state) x)
    | Vect x ->
      if List.length x < 1 then
      traises "Cannot infer type of empty vector literal" state.stack
      else let head = eval (List.hd x) state in
      let head_type = typeof head in
      let rest = (List.map (fun x -> eval x state) x) in
      (* Check if the rest of the vector literal contains value of the same type as
      the first element, if not, fail because vectors must be homogeneous *)
      List.iter (fun y -> dyncheck (typeof y) head_type) rest;
      EvtVect(head_type, Array.of_list rest)
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
      let eargs = Array.map (fun x -> eval x state) args in
      let prim = Primitives.get_primitive_function (match (Dict.get name Primitives.ocaml_table) with
          | None -> iraise (Fatal "Unbound primitive. This should never happen")
          | Some p -> p) in
      (try prim eargs with InternalError (loc, err, _) -> raise (InternalError(loc, err, state.stack)))
    (* Eval a sequence of expressions but return the last *)
    | Sequence (e1, e2) ->
      let _ = eval e1 state in
      eval e2 state
  in
  if state.verbosity >= 2 then
    print_message
      ~color:T.Cyan
      ~loc:Nowhere
      "Evaluates to at depth"
      (Printf.sprintf "%d\n%s\n"
         (Estack.depth_of_stack state.stack)
         (show_evt evaluated))
  else ();
  evaluated

and eval_binop (k: binop) (x: expr) (y: expr) state =
  let ev1 = eval x state in
  let t1 = typeof ev1 in
  match k with
  | Getkey ->
    let key = (match y with Symbol z -> z | _ -> iraise (Fatal "Dictionary access"))
    and ed = unpack_dict (ev1) in
    (match Dict.get key ed with
     | None -> iraise (DictError "key not found")
     | Some (LazyExpression z) -> eval z state
     | Some z -> z)
  | Cons ->
    let ev2 = eval y state in
    let ls = unpack_list ev2 in
    (match ls with
     | [] -> EvtList [ ev1 ]
     | lss -> EvtList (ev1 :: lss))
  | Concat ->
    let ev2 = eval y state in let t2 = typeof ev2 in
    (match (t1, t2) with
     | TString, TString -> EvtString ((unpack_string ev1) ^ (unpack_string ev2))
     | TList, TList -> EvtList ((unpack_list ev1) @ (unpack_list ev2))
     | _ -> iraises (TypeError (Printf.sprintf "Cannot concatenate a two values of type %s and %s"
                                  (show_typeinfo t1) (show_typeinfo t2))) state.stack )
  | Compose ->
    let ev2 = eval y state in
    let ef1 = ev2 and ef2 = ev1 in
    dyncheck (typeof ef1) TLambda; dyncheck (typeof ef2) TLambda;
    let params1 = Values.findevtparams ef1 in
    let appl1 = Expr.apply_from_exprlist (Expr.symbols_from_strings params1) y in
    eval (Expr.lambda_of_paramlist params1 (Apply (x, appl1))) state

  | MakeComplex -> Numericalp.makecomplex [|ev1; (eval y state)|]
  | Plus  ->  Numericalp.add [|ev1; (eval y state)|]
  | Sub  ->   Numericalp.sub [|ev1; (eval y state)|]
  | Div  ->   Numericalp.div [|ev1; (eval y state)|]
  | Mult  ->  Numericalp.mult [|ev1; (eval y state)|]
  | Topow ->  Numericalp.float_binop Owl_base_maths.pow [|ev1; (eval y state)|]
  | And  -> bool_binop (ev1, (eval y state)) ( && )
  | Or -> bool_binop (ev1, (eval y state)) ( || )
  | Eq -> EvtBool (compare_evt ev1 (eval y state) = 0)
  | Gt -> EvtBool (compare_evt ev1 (eval y state) > 0)
  | Lt -> EvtBool (compare_evt ev1 (eval y state) < 0)
  | Ge -> EvtBool (compare_evt ev1 (eval y state) >= 0)
  | Le -> EvtBool (compare_evt ev1 (eval y state) <= 0)


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
        (Values.show_evt_fancy evaluated)
        (show_typeinfo (Typecheck.typeof evaluated))
    else ();
    (evaluated, state)
  | Def dl ->
    let (islazyl, idel, vall) = unzip3 dl in
    let ovall = (List.map (Optimizer.iterate_optimizer) vall) in
    let odl = zip3 islazyl idel ovall in
    (* Print the definitions if verbosity is enough and they were optimized *)
    if ovall = vall then () else
    if state.verbosity >= 1 then print_message ~loc:(Nowhere) ~color:T.Yellow "After AST optimization"
        (Printf.sprintf "\n%s" (show_command (Def odl))) else ();
    let newstate = eval_assignment_list odl state in
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
    (* Inclue a file as a module: *)
    (* Extract the module name from the file name. *)
    (* //TODO fix invalid names *)
    let modulename = (match m with
        | Some m -> m
        | None -> Filename.remove_extension f |> Filename.basename |> String.capitalize_ascii) in
    (* If the file name is relative, load it from the currently evaluated
    file position and not from the CWD *)
    let file_in_scope = if not (Filename.is_relative f) then f else
        Filename.concat (dirscope) f in
    (* Get the command list *)
    let cmdlist = (Parsedriver.read_file file_in_scope) in
    (* Infer the purity *)
    let resulting_state = Puritycheck.infer_command_list cmdlist {state with purityenv = []} in
    (* Evaluate the contents of the file with inside new environments
      and get the resulting state *)
    let _, resulting_state = eval_command_list cmdlist 
        { resulting_state with env = [] } dirscope in
    (* Create a dictionary (module) from the resulting state's value environment *)
    let newmodule = EvtDict resulting_state.env in
    (* Return a new state by binding the resulting state environments to the module name *)
    (EvtUnit, {
      state with
      env = (Dict.insert state.env modulename newmodule);
      purityenv = (Dict.insert state.purityenv modulename (PurityModule resulting_state.purityenv)) })
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
  | Openmodule m -> 
    match lookup m state with 
    | EvtDict d -> (EvtUnit, { state with env = d @ state.env})
    | _ -> traise "Cannot open a value that is not a module"