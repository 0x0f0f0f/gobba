open Types
open Typecheck
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

(** Check if a dict contains a key *)
let getkey args =
  let (key, ed) = (match args with
      | [key; d] -> (unpack_string key, unpack_dict d)
      | _ -> iraise WrongPrimitiveArgs) in
  if not (Dict.exists key ed) then iraise (DictError "key not found") else
    Dict.get key ed

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

let table = [
  ("insert",         Primitive (insert_dict, ("insert", 3, Pure)));
  ("remove",         Primitive (delete_dict, ("remove", 2, Pure)));
  ("haskey",         Primitive (haskey, ("haskey", 2, Pure)));
  ("getkey",         Primitive (getkey, ("getkey", 2, Pure)));
  ("getkeys",        Primitive (getkeys, ("getkeys", 1, Pure)));
  ("getvalues",      Primitive (getvalues, ("getvalues", 1, Pure)));
  ("dictfromlists",  Primitive (dict_from_lists, ("dictfromlists", 2, Pure)));
  ("filterkeys",     Primitive (filterkeys, ("filterkeys", 2, Pure)))
]
