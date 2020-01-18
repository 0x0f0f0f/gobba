open Types
open Errors
open Util

(** "level out" the purity of two values *)
let level_purity a b = match (a, b) with
  | Impure, _ | _, Impure             -> Impure
  | PurityModule x, _ | _, PurityModule x         -> PurityModule x
  | Pure, Numerical | Numerical, Pure -> Pure
  | Numerical, Numerical              -> Numerical
  | Uncertain, _ | _, Uncertain       -> Uncertain
  | Pure, Pure                        -> Pure

(** Infer the purity of an expression. Note: this is a naive approach.
  This function is an abstract interpretation of expressions over primitives and environments.
  @param pt The primitives table
  @param state The current computation state
  @param is_in_lambda If inside a lambda, the list of parameters
   *)
let rec infer e state : puret =
  let state = { state with stack = (Estack.push_stack state.stack e) } in
  let inferp x = infer x state in
  let inferp2 a b = level_purity (inferp a) (inferp b) in
  let inferpl ls =
    let apl = List.map inferp ls in
    List.fold_left level_purity Numerical apl in
  match e with
  | NumInt _ | NumFloat _ | NumComplex _ | Unit -> Numerical
  | Boolean _ | String _ | Character _ -> Pure
  | Not a -> inferp a
  (* Expressions with lists of expressions *)
  | Vect l  -> inferpl l
  | List l  -> inferpl l
  (* Dictionaries contain key value pairs, level them out *)
  | Dict (l) ->
    let _, names, values = unzip3 l in
    let apl = List.map (fun v -> infer v { state with purity = Impure}) values in
    PurityModule (zip names apl)
  | Binop(Getkey, d, k) ->
    let kk = match k with Symbol s -> s | _ -> traises "Properties accessed must be a symbol" state.stack in
    let dp = inferp d in
    (match dp with
      | PurityModule m -> lookup_env kk m Numerical
      | _ -> traises "Cannot access a property of a value that is not a dictionary" state.stack)
  | Purity (allowed, body) ->
    if (state.purity = Pure || state.purity = Numerical) && allowed = Impure then
      iraise (PurityError ("Cannot enter an " ^ (show_puret allowed) ^
      " context from a " ^ (show_puret state.purity) ^ " one!"))
      else infer body { state with purity = allowed }
  (* Infer from all the other binary operators and sequences *)
  | Binop(_, a, b) | Sequence (a, b) -> inferp2 a b
  | Lambda(_, b) -> infer b { state with purity = Impure}
  | IfThenElse(g, t, f) ->
    let pg = infer g state and pt = infer t state and pf = infer f state in
    level_purity pg (level_purity pt pf)
  | Let(assignments, body) ->
    let newstate = infer_assignment_list assignments state in
    infer body newstate
  | Symbol s -> lookup s state
  | Apply (f, arg) ->
    let fp = inferp f and argp = inferp arg in
    if state.purity <> Impure && fp = Impure then
        iraises (PurityError
          (Printf.sprintf "Tried to apply a %s value in a %s state" (show_puret fp) (show_puret state.purity)))
          state.stack
    else level_purity fp argp
  | ApplyPrimitive ((name, numparams, purity), args) ->
      if List.length args != numparams then (iraise (Fatal "Primitive Application Error"))
      else if state.purity <> Impure && purity = Impure then
        iraises
          (PurityError ("Tried to apply an impure primitive in a pure block: " ^ name))
          state.stack
      else purity

and lookup_env (name: ide) (table: purityenv_type) fallback =
  match Dict.get name table with
  | None -> fallback
  | Some (p) -> p


and lookup (name: ide) (state: evalstate) : puret =
  let prim_purity = lookup_env name Primitives.purity_table (Uncertain) in
  match prim_purity with
    (* Symbol is either a parameter or an unbound variable *)
    | Uncertain -> lookup_env name state.purityenv (Numerical)
    | e -> e

and infer_assignment state (_, name, value) : evalstate =
  (* Return a new state, updating the purityenv with the new binding, infered from the value (where Impure contexts are allowed) *)
  { state with purityenv = (Dict.insert state.purityenv name ((infer value { state with purity = Impure }))) }

and infer_assignment_list assignments state : evalstate =
  match assignments with
  | [] -> state
  | (islazy, name, value)::xs ->
    let newstate = infer_assignment state (islazy, name, value) in
    (infer_assignment_list xs newstate)
