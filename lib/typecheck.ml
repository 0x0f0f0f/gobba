open Types

let terr t = raise (TypeError ("expected a value of type: " ^ t))

let expect t e = if t = e
  then ()
  else terr t

let typecheck (x: evt) (t: string) = match x with
  | EvtInt _      -> expect t "int"
  | EvtBool _     -> expect t "bool"
  | EvtString _   -> expect t "string"
  | EvtUnit       -> expect t "unit"
  | EvtList _     -> expect t "list"
  | EvtDict _     -> expect t "dict"
  | Closure _     -> expect t "fun"
  | RecClosure _  -> expect t "fun"
  | PrimitiveAbstraction _  -> expect t "fun"

(** Unpacking functions: extract a value or throw an err *)

let unpack_int x = (match x with EvtInt i -> i | _ -> terr "int")
let unpack_bool x = (match x with EvtBool i -> i | _ -> terr "bool")
let unpack_string x = (match x with EvtString i -> i | _ -> terr "string")
let unpack_list x = (match x with EvtList i -> i | _ -> terr "list")
let unpack_dict x = (match x with EvtDict i -> i | _ -> terr "dict")
let unpack_closure x = (match x with Closure (p, b, e) -> (p,b,e) | _ -> terr "fun")
let unpack_recclosure x = (match x with
    | RecClosure (i, p, b, e) -> (i,p,b,e)
    | _ -> terr "fun")
let unpack_anyfun x = match x with
  | RecClosure (i, p, b, e) -> (i,p,b,e)
  | Closure (p, b, e) -> ("",p,b,e)
  | _ -> terr "fun"

(* Check if first elem of tuple is an allowed type for dict key and return tuple *)
let isvalidkey (x, y) = ((match x with
    | EvtInt _ -> x
    | EvtBool _ -> x
    | EvtString _ -> x
    | _ -> failwith "value not allowed as dictionary key"), y)