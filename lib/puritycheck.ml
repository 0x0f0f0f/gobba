open Types
open Util

(** "level out" the purity of two values *)
let level_purity a b = match (a, b) with
  | Impure, _ | _, Impure             -> Impure
  | Pure, Numerical | Numerical, Pure -> Pure
  | Numerical, Numerical              -> Numerical
  | Uncertain, _ | _, Uncertain       -> Uncertain
  | Pure, Pure                        -> Pure

(** Infer the purity of an expression. Note: this is a naive approach.
  This function is an abstract interpretation of expressions over primitives and environments.
  @param pt The primitives table
  @param paraml If inside a lambda, the list of parameters
  @param env An environment, needed to infer whether lazy expressions are pure or not *)
let rec infer e state : puret =
  let state = { state with stack = (push_stack state.stack e) } in
  let inferp x = infer x state in
  let inferp2 a b = level_purity (inferp a) (inferp b) in
  let inferpl ls =
    let apl = List.map inferp ls in
    List.fold_left level_purity Numerical apl in
  match e with
  | NumInt _ | NumFloat _ | NumComplex _ | Unit -> Numerical
  | Boolean _ | String _ -> Pure
  | Not a -> inferp a 
  (* Expressions with lists of expressions *)
  | List l | Sequence l -> inferpl l
  (* Dictionaries contain key value pairs, level them out *)
  | Dict (l) -> let _, vl = unzip l in inferpl vl
  | Purity (allowed, body) ->
    if isstrictlypure state.purity && isimpure allowed then
      iraise (PurityError ("Cannot enter an " ^ (show_puret allowed) ^
      " context from a " ^ (show_puret state.purity) ^ " one!"))
      else infer body { state with purity = allowed }
  (* Infer from all the binary operators *)
  | Compose (a, b) | Plus (a, b) | Sub (a, b) | Cons (a, b)
  | Concat (a, b) | Eq (a, b) | Lt (a, b) | Ge (a, b) | Le (a, b)
  | Gt (a, b) | And (a, b) | Or (a, b)
  | Mult (a, b) | Div (a, b) -> inferp2 a b
  | Lambda(_, b) -> infer b state
  | IfThenElse(g, t, f) ->
    let pg = infer g state and pt = infer t state and pf = infer f state in
    level_purity pg (level_purity pt pf)
  | Let(assignments, body) ->
    let newstate = infer_assignment_list assignments state in
    infer body newstate
  | Symbol s -> lookup s state
  | Apply (f, arg) ->
    let fp = inferp f and argp = inferp arg in
    if ispure state.purity && isimpure fp then
        iraises (PurityError
          (Printf.sprintf "Tried to apply a %s value in a %s state" (show_puret fp) (show_puret state.purity)))
          state.stack
    else level_purity fp argp
  | ApplyPrimitive ((name, numparams, purity), args) ->
      if List.length args != numparams then (iraise (Fatal "Primitive Application Error"))
      else if ispure state.purity && isimpure purity then
        iraises
          (PurityError ("Tried to apply an impure primitive in a pure block: " ^ name))
          state.stack
      else purity

and lookup (name: ide) (state: evalstate) : puret =
  match Dict.get name Primitives.purity_table with
    | None -> (match Dict.get name state.purityenv with
      | None -> Numerical (* Value may be unbound or in parameters *)
      | Some purity -> purity)
    | Some purity -> purity

and infer_assignment state (_, name, value) : evalstate =
  (* Return a new state, updating the purityenv with the new binding, infered from the value (where Impure contexts are allowed) *)
  { state with purityenv = (Dict.insert state.purityenv name (infer value { state with purity = Impure })) }

and infer_assignment_list assignments state : evalstate =
  match assignments with
  | [] -> state
  | (islazy, name, value)::xs ->
    let newstate = infer_assignment state (islazy, name, value) in
    (infer_assignment_list xs newstate)
