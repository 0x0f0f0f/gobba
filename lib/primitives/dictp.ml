open Types
open Errors
open Typecheck
open Primutil
open Util

(** Insert a key-value pair in a dictionary *)
let insert_dict args =
  let (k, v, d) = (match args with
      | [k; v; d] -> (unpack_string k, v, unpack_dict d)
      | _ -> iraise WrongPrimitiveArgs) in
  EvtDict ((k, v) :: (Dict.delete k d))


(** Remove a key-value pair from a dictionary *)
let delete_dict args =
  let (key, ed) = (match args with
      | [key; d] -> (unpack_string key, unpack_dict d)
      | _ -> iraise WrongPrimitiveArgs) in
  if not (Dict.exists key ed) then iraise (DictError "key not found") else
    EvtDict (Dict.delete key ed)

(** Check if a key-value pair is in a dictionary *)
let haskey args =
  let (key, ed) = (match args with
      | [key; d] -> (unpack_string key, unpack_dict d)
      | _ -> iraise WrongPrimitiveArgs) in
  EvtBool(Dict.exists key ed)

(** Get a list of the keys *)
let getkeys args =
  let ed = (match args with
      | [d] ->  unpack_dict d
      | _ -> iraise WrongPrimitiveArgs) in
  EvtList(List.map (fun x -> EvtString x) (Dict.getkeys ed))

(** Get a list of the values *)
let getvalues args =
  let ed = (match args with
      | [d] ->  unpack_dict d
      | _ -> iraise WrongPrimitiveArgs) in
  EvtList(Dict.getvalues ed)


(** Check if a dict contains a key *)
let filterkeys args =
  let (kll, ed) = (match args with
      | [kl; d] -> (List.map (unpack_string) (unpack_list kl), unpack_dict d)
      | _ -> iraise WrongPrimitiveArgs) in
  EvtDict(Dict.filter kll ed)

(** Build a dictionary from two lists *)
let dict_from_lists args =
  let (kl, vl) = (match args with
    | [kl; vl] -> (List.map (unpack_string) (unpack_list kl), unpack_list vl)
    | _ -> iraise WrongPrimitiveArgs) in
  EvtDict(zip kl vl)

let mapstr =
{| fun f l ->
    let keys = Dict:getkeys l and values = Dict:getvalues l in
    Dict:dictfromlists keys (List:map f values)
|}

let foldlstr =
{| fun f z l ->
  let aux = fun f z kl vl ->
    if kl = [] && vl = [] then z else
    aux f (f z (List:head vl)) (List:tail kl) (List:tail vl)
  in aux f z (Dict:getkeys l) (Dict:getvalues l)
|}

let foldrstr =
{| fun f z l ->
  let aux = fun f z kl vl ->
    if kl = [] && vl = [] then z else
    f (List:head vl) (aux f z (List:tail kl) (List:tail vl))
  in aux f z (Dict:getkeys l) (Dict:getvalues l)
|}

let filterstr =
{| fun pred l ->
  let aux = fun f kl vl acc ->
      if kl = [] && vl = [] then acc
      else if f (List:head vl) then
        aux f (List:tail kl) (List:tail vl) (Dict:insert (List:head kl) (List:head vl) acc)
    else aux f (List:tail kl) (List:tail vl) acc in
  aux pred (Dict:getkeys l) (Dict:getvalues l) {}

|}


let lambda_table = [
  ("map", (lambda_of_string "map" mapstr));
  ("filter", (lambda_of_string "filter" filterstr));
  ("foldl", (lambda_of_string "foldl" foldlstr));
  ("foldr", (lambda_of_string "foldr" foldrstr))
]



let table = [
  ("insert",         Primitive (insert_dict, ("insert", 3, Pure)));
  ("remove",         Primitive (delete_dict, ("remove", 2, Pure)));
  ("haskey",         Primitive (haskey, ("haskey", 2, Pure)));
  ("getkeys",        Primitive (getkeys, ("getkeys", 1, Pure)));
  ("getvalues",      Primitive (getvalues, ("getvalues", 1, Pure)));
  ("dictfromlists",  Primitive (dict_from_lists, ("dictfromlists", 2, Pure)));
  ("filterkeys",     Primitive (filterkeys, ("filterkeys", 2, Pure)))
]
