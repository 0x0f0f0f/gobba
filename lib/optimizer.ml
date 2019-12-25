open Types

let rec optimize (e: expr) : expr = match e with
  | Plus(Integer x, Integer y) -> Integer (x + y) (* Autoreduce constants *)
  | Sub(Integer x, Integer y) -> Integer (x - y)
  | Mult(Integer x, Integer y) -> Integer (x * y)
  | Eq(Integer x, Integer y) -> Boolean (x == y)
  | Gt(Integer x, Integer y) -> Boolean (x > y)
  | Lt(Integer x, Integer y) -> Boolean (x < y)
  | And(Boolean x, Boolean y) -> Boolean (x && y)
  | Or(Boolean x, Boolean y) -> Boolean (x || y)
  | Eq(x, y) -> Eq (optimize x, optimize y)
  | Gt(x, y) -> Gt (optimize x, optimize y)
  | Lt(x, y) -> Lt (optimize x, optimize y)
  | Not(Boolean x) -> Boolean (not x)
  | Plus(x, y) ->  Plus (optimize x, optimize y)
  | Sub(x, y) ->  Sub (optimize x, optimize y)
  | Mult(x, y) -> Mult (optimize x, optimize y)
  | And(x, y) ->  And (optimize x, optimize y)
  | Or(x, y) ->   Or (optimize x, optimize y)
  | Not(x) -> Not (optimize x)
  | List(l) -> List(List.map optimize l)
  | Dict(d) -> Dict(List.map (fun (k, v) -> (optimize k, optimize v)) d)
  | IfThenElse(guard, iftrue, iffalse) ->
    (* Short circuit an if that is always true or false *)
    let og = optimize guard in
    if og = Boolean true then optimize iftrue
    else if og = Boolean false then optimize iffalse
    else IfThenElse(og, optimize iftrue, optimize iffalse)
  | Lambda(params, body) -> Lambda(params, optimize body)
  | Let(declarations, body) -> optimize_let declarations body false
  | Letlazy(declarations, body) -> optimize_let declarations body true
  | Letrec(ident, dec, body) -> Letrec(ident, optimize dec, optimize body)
  | Letreclazy(ident, dec, body) -> Letreclazy(ident, optimize dec, optimize body)
  | Apply(func, params) -> Apply(func, (List.map optimize params))
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