open Types

let rec optimize (e: expr) : expr = match e with
  | Eq(NumInt x, NumInt y) -> Boolean (x == y)
  | Gt(NumInt x, NumInt y) -> Boolean (x > y)
  | Lt(NumInt x, NumInt y) -> Boolean (x < y)
  | Eq(x, y) -> Eq (optimize x, optimize y)
  | Gt(x, y) -> Gt (optimize x, optimize y)
  | Lt(x, y) -> Lt (optimize x, optimize y)
  | Plus(x, y) ->  Plus (optimize x, optimize y)
  | Sub(x, y) ->  Sub (optimize x, optimize y)
  | Mult(x, y) -> Mult (optimize x, optimize y)
  | Div(x, y) -> Div (optimize x, optimize y)
  | Apply(a, b) ->  Apply(optimize a, optimize b)
  | List(l) -> List(List.map optimize l)
  | Dict(d) -> Dict(List.map (fun (k, v) -> (optimize k, optimize v)) d)
  | Lambda(params, body) -> Lambda(params, optimize body)
  | Let(declarations, body) -> optimize_let declarations body false
  | Letlazy(declarations, body) -> optimize_let declarations body true
  | Letrec(ident, dec, body) -> Letrec(ident, optimize dec, optimize body)
  | Letreclazy(ident, dec, body) -> Letreclazy(ident, optimize dec, optimize body)
  (* Propositional Calculus optimizations *)
  | And(Boolean x, Boolean y) -> Boolean (x && y)
  | Or(Boolean x, Boolean y) -> Boolean (x || y)
  | Not(Not(x)) -> optimize x
  | Not(Boolean x) -> Boolean (not x)
  | Not(And(a, b)) -> Or(Not (optimize a), Not (optimize b))    (* DeMorgan *)
  | Not(Or(a, b)) -> And(Not (optimize a), Not (optimize b))    
  | Or(a, And (Not aa, b)) ->           (* Complement *)
    let oa = optimize a and oaa = optimize aa and ob = optimize b in
    if oa = oaa then Or(oa, ob)
    else Or(oa, And (Not oaa, ob))
  | And(a, Or (Not aa, b)) -> 
    let oa = optimize a and oaa = optimize aa and ob = optimize b in
    if oa = oaa then And(oa, ob)
    else And(oa, Or (Not oaa, ob))
  | Or(a, And (aa, b)) ->               (* Absorb *)
    let oa = optimize a and oaa = optimize aa and ob = optimize b in
    if oa = oaa then oa
    else Or(oa, And (oaa, ob))
  | And(a, Or (aa, b)) ->               (* Absorb *)
    let oa = optimize a and oaa = optimize aa and ob = optimize b in
    if oa = oaa then oa
    else And(oa, Or (oaa, ob))
  | Not(x) -> Not (optimize x)
  | And(x, y) ->  And (optimize x, optimize y)
  | Or(x, y) ->   Or (optimize x, optimize y)
  | IfThenElse(guard, iftrue, iffalse) ->
    (* Short circuit an if that is always true or false *)
    let og = optimize guard in
    if og = Boolean true then optimize iftrue
    else if og = Boolean false then optimize iffalse
    else IfThenElse(og, optimize iftrue, optimize iffalse)
  | _ -> e
and optimize_let declarations body islazy =
  let od = List.map (fun (i, v) -> (i, optimize v)) declarations in
  let ob = optimize body in
  if List.length od = 1 then
    let (ident, value) = List.hd od in
    if ob = Symbol ident
    then optimize value
    else if islazy then Letlazy(od, ob) else Let(od, ob)
  else if islazy then Letlazy(od, ob) else Let(od, ob)

(** Apply the optimizer again and again on an expression until it
    is fully reduced and ready to be evaluated *)
let rec iterate_optimizer e =
  let oe = optimize e in
  if oe = e then e (* Bottoms out *)
  else iterate_optimizer oe