open Types

let rec optimize (e: expr) : expr = match e with
  | Sum(Integer x, Integer y) -> Integer (x + y) (* Autoreduce constants *)
  | Sub(Integer x, Integer y) -> Integer (x - y)
  | Mult(Integer x, Integer y) -> Integer (x * y)
  | Eq(Boolean x, Boolean y) -> Boolean (x == y)
  | Eq(x, y) -> Eq (optimize x, optimize y)
  | And(Boolean x, Boolean y) -> Boolean (x && y) (* Optimize bool primitives *)
  | Or(Boolean x, Boolean y) -> Boolean (x || y)
  | Not(Boolean x) -> Boolean (not x)
  | Sum(x, y) ->  Sum (optimize x, optimize y)
  | Sub(x, y) ->  Sub (optimize x, optimize y)
  | Mult(x, y) -> Mult (optimize x, optimize y)
  | And(x, y) ->  And (optimize x, optimize y)
  | Or(x, y) ->   Or (optimize x, optimize y)
  | Not(x) -> Not (optimize x)
  | IfThenElse(guard, iftrue, iffalse) ->
      (* Short circuit an if that is always true or false *)
      let og = optimize guard in
      if og = Boolean true then optimize iftrue
      else if og = Boolean false then optimize iffalse
      else IfThenElse(og, optimize iftrue, optimize iffalse)
  | Lambda(params, body) -> Lambda(params, optimize body)
  | Let(declarations, body) -> optimize_let declarations body false
  | Letlazy(declarations, body) -> optimize_let declarations body true
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