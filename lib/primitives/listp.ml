open Types
open Errors
open Typecheck
open Util
open Primutil

let head args  =
  if Array.length args > 1 then iraise WrongPrimitiveArgs else
    let ls= unpack_list args.(0) in
    (match ls with
     | [] -> iraise (ListError "empty list")
     | v::_ -> v )

let length args  =
  if Array.length args > 1 then iraise WrongPrimitiveArgs else
    let ls= unpack_list args.(0) in
    EvtInt(List.length ls)

let getat args =
  let at, ls = (match args with
      | [|EvtInt a; EvtList l|] -> (a,l)
      | _ -> iraise WrongPrimitiveArgs) in
  let curln = List.length ls in
  let nat = if at < 0 then curln + at else at in
  if curln <= nat || nat < 0 then iraise IndexOutOfBounds
  else List.hd (drop nat ls)

let tail args =
  if Array.length args > 1 then iraise WrongPrimitiveArgs else
    let ls= unpack_list args.(0) in
    (match ls with
     | [] -> iraise (ListError "empty list")
     | _::r -> EvtList r)

let mem args =
  let (elem, ls)= (match args with
      | [|elem; ls|] -> (elem, unpack_list ls)
      | _ -> iraise WrongPrimitiveArgs) in
  EvtBool(List.mem elem ls)

let mapstr =
{|
  fun f l ->
    let aux = fun f l ->
      (if l = [] then l else (f (List:head l))::(aux f (List:tail l)))
    in aux f l
|}

let filterstr =
{|
  fun pred l ->
    let aux = fun f l ->
    if l = [] then l else
      let h = (List:head l) in if f h then
        h::(aux f (List:tail l))
        else (aux f (List:tail l)) in
    aux pred l
|}


let foldlstr =
{| fun f z l ->
    let aux = fun f z l ->
        if l = [] then z else
        aux f (f z (List:head l)) (List:tail l)
    in aux f z l
|}


let foldrstr =
{| fun f z l ->
    let aux = fun f z l ->
      if l = [] then z else
      f (List:head l) (aux f z (List:tail l))
    in aux f z l
|}

let map2str =
{| fun f l1 l2 ->
    let aux = fun f l1 l2 ->
        if List:length l1 != List:length l2 then
          failwith "lists are not of equal length"
        else if l1 = [] && l2 = [] then l1 else
        (f (List:head l1) (List:head l2)) :: (aux f (List:tail l1) (List:tail l2))
    in aux f l1 l2
|}

let mapnstr =
{| fun f lists ->
  if (not (lists = [])) then
    if List:mem [] lists then
     []
    else
      f (map List:head lists) :: mapn f (map List:tail lists)
  else []
|}


let lambda_table = [
  ("map", (lambda_of_string "map" mapstr));
  ("map2", (lambda_of_string "map2" map2str));
  ("mapn", (lambda_of_string "mapn" mapnstr));
  ("filter", (lambda_of_string "filter" filterstr));
  ("foldl", (lambda_of_string "foldl" foldlstr));
  ("foldr", (lambda_of_string "foldr" foldrstr))
]

let table = [
  ("head",    Primitive (head, ("head", [|"list"|], Pure)));
  ("tail",    Primitive (tail, ("tail", [|"list"|], Pure)));
  ("mem",     Primitive (mem, ("mem", [|"element";"list"|], Pure)));
  ("length",  Primitive (length, ("length", [|"list"|], Pure)));
  ("nth",      Primitive (getat, ("nth", [|"position"; "list"|], Pure)));
]
