open Types

let expect t e = if t = e
  then ()
  else raise (TypeError ("expected a value of type: " ^ t))

let typecheck (x: evt) (t: string) = match x with
  | EvtInt _      -> expect t "int"
  | EvtBool _     -> expect t "bool"
  | EvtString _   -> expect t "string"
  | EvtUnit       -> expect t "unit"
  | EvtList _     -> expect t "list"
  | EvtDict _     -> expect t "dict"
  | Closure _     -> expect t "fun"
  | RecClosure _  -> expect t "fun"