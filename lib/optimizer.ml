open Types

(** Normalize an AST expression *)
let rec optimize (e: expr) : expr = match e with
  (* Redundant cases *)
  | Unit | NumInt _ | NumFloat _ | NumComplex _ | Character _
  | String _ | Symbol _ | Boolean _  -> e
  | Purity(_, b) -> optimize b
  | Binop(Eq, NumInt x, NumInt y) -> Boolean (x == y)
  | Binop(Gt, NumInt x, NumInt y) -> Boolean (x > y)
  | Binop(Lt, NumInt x, NumInt y) -> Boolean (x < y)
  | List(l) -> List(List.map optimize l)
  | Vect(l) -> Vect(List.map optimize l)
  | Dict(d) -> Dict(List.map (fun (l, k, v) -> (l, k, optimize v)) d)
  | Lambda(params, body) -> Lambda(params, optimize body)
  | Let(declarations, body) -> optimize_let declarations body
  (* Propositional Calculus optimizations *)
  | Binop(And, Boolean x, Boolean y) -> Boolean (x && y)
  | Binop(Or, Boolean x, Boolean y) -> Boolean (x || y)
  | Not(Not(x)) -> optimize x
  | Not(Boolean x) -> Boolean (not x)
  | Not(Binop(And, a, b)) -> Binop(Or, Not (optimize a), Not (optimize b))    (* DeMorgan *)
  | Not(Binop(Or, a, b)) -> Binop(And, Not (optimize a), Not (optimize b))
  | Binop(Or, a, Binop(And, Not aa, b)) ->           (* Complement *)
    let oa = optimize a and oaa = optimize aa and ob = optimize b in
    if oa = oaa then Binop(Or, oa, ob)
    else Binop(Or, oa, Binop(And, Not oaa, ob))
  | Binop(And, a, Binop(Or, Not aa, b)) ->
    let oa = optimize a and oaa = optimize aa and ob = optimize b in
    if oa = oaa then Binop(And, oa, ob)
    else Binop(And, oa, Binop(Or, Not oaa, ob))
  | Binop(Or, a, Binop(And, aa, b)) ->               (* Absorb *)
    let oa = optimize a and oaa = optimize aa and ob = optimize b in
    if oa = oaa then oa
    else Binop(Or, oa, Binop(And, oaa, ob))
  | Binop(And, a, Binop(Or, aa, b)) ->               (* Absorb *)
    let oa = optimize a and oaa = optimize aa and ob = optimize b in
    if oa = oaa then oa
    else Binop(And, oa, Binop(Or, oaa, ob))
  | Not(x) -> Not (optimize x)
  | IfThenElse(guard, iftrue, iffalse) ->
    (* Short circuit an if that is always true or false *)
    let og = optimize guard in
    if og = Boolean true then optimize iftrue
    else if og = Boolean false then optimize iffalse
    else IfThenElse(og, optimize iftrue, optimize iffalse)
  (* Binary cases *)
  | Binop(kind, x, y) -> Binop(kind, optimize x, optimize y)
  | Apply(a, b) ->  Apply(optimize a, optimize b)
  | Sequence(a, b) ->
    let oa = optimize a and ob = optimize b in
    Sequence (oa, ob)
  | ApplyPrimitive(p, ls) -> ApplyPrimitive(p, Array.map optimize ls)
and optimize_let declarations body =
  let od = List.map (fun (l, i, v) -> (l, i, optimize v)) declarations in
  let ob = optimize body in
  Let(od, ob)

(** Apply the optimizer again and again on an expression until it
    is fully reduced and ready to be evaluated *)
let rec iterate_optimizer e =
  let oe = optimize e in
  if oe = e then e (* Bottoms out *)
  else iterate_optimizer oe