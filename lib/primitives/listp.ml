open Types
open Errors
open Typecheck
open Util

let head args  =
  if List.length args > 1 then iraise WrongPrimitiveArgs else
    let ls= unpack_list (List.hd args) in
    (match ls with
     | [] -> iraise (ListError "empty list")
     | v::_ -> v )

let length args  =
  if List.length args > 1 then iraise WrongPrimitiveArgs else
    let ls= unpack_list (List.hd args) in
    EvtInt(List.length ls)

let getat args =
  let at, ls = (match args with
      | [EvtInt a; EvtList l] -> (a,l)
      | _ -> iraise WrongPrimitiveArgs) in
  let curln = List.length ls in
  let nat = if at < 0 then curln + at else at in
  if curln <= nat || nat < 0 then iraise IndexOutOfBounds
  else List.hd (drop nat ls)

let tail args =
  if List.length args > 1 then iraise WrongPrimitiveArgs else
    let ls= unpack_list (List.hd args) in
    (match ls with
     | [] -> iraise (ListError "empty list")
     | _::r -> EvtList r)

let mem args =
  let (elem, ls)= (match args with
      | [elem; ls] -> (elem, unpack_list ls)
      | _ -> iraise WrongPrimitiveArgs) in
  EvtBool(List.mem elem ls)

let table = [
  ("head",    Primitive (head, ("head", 1, Pure)));
  ("tail",    Primitive (tail, ("tail", 1, Pure)));
  ("mem",     Primitive (mem, ("mem", 2, Pure)));
  ("length",  Primitive (length, ("length", 2, Pure)));
  ("nth",      Primitive (getat, ("nth", 2, Pure)));
]
