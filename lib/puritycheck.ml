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
  let inferp x = infer x state in
  let inferp2 a b = level_purity (inferp a) (inferp b) in
  let inferpl ls =
    let apl = List.map inferp ls in
    List.fold_left level_purity Numerical apl in
  match e with
  | NumInt _ | NumFloat _ | NumComplex _ -> Numerical
  | Purity (allowed, body) ->
    if isstrictlypure state.purity && isimpure allowed then
      iraise (PurityError ("Cannot enter an " ^ (show_puret allowed) ^
      " context from a " ^ (show_puret state.purity) ^ " one!"))
      else infer body { state with purity = allowed }
  | Sequence l -> inferpl l
  (* Infer from all the binary operators *)
  | Compose (a, b) | Plus (a, b) | Sub (a, b)
  | Mult (a, b) | Div (a, b) -> inferp2 a b
  | Lambda(_, b) -> infer b state
  | Let(assignments, body) ->
    let new_penv = infer_assignment_list assignments state in
    infer body { state with purityenv = new_penv }
  | Symbol s -> lookup s state
  | Apply (f, arg) ->
    let fp = inferp f and argp = inferp arg in
    if ispure state.purity && isimpure fp then
        iraise (PurityError ("Tried to apply a " ^ (show_puret fp) ^ " value in a " ^ (show_puret state.purity) ^ " state"))
    else level_purity fp argp
  | ApplyPrimitive ((name, numparams, purity), args) ->
      if List.length args != numparams then (iraise (Fatal "Primitive Application Error"))
      else if ispure state.purity && isimpure purity then
        iraise (PurityError ("Tried to apply an impure primitive in a pure block: " ^ name))
      else purity
  | _ -> Pure

and lookup (name: ide) (state: evalstate) : puret =
  match Dict.get name Primitives.purity_table with
    | None -> (match Dict.get name state.purityenv with
      | None -> Numerical (* Value may be unbound or in parameters *)
      | Some purity -> purity)
    | Some purity -> purity

and infer_assignment state (_, _, value) = infer value state

and infer_assignment_list assignments state =
  match assignments with
  | [] -> []
  | (islazy, name, value)::xs ->
    let assignment_purity = infer_assignment state (islazy, name, value) in
    let newstate = { state with purityenv = Dict.insert state.purityenv name assignment_purity } in
    (name, assignment_purity)::(infer_assignment_list xs newstate)
