open Types
open Typecheck
open Util

(** Insert a key-value pair in a dictionary *)
let insert_dict args =
  let (k, v, d) = (match args with
      | [k; v; d] -> (k, v, unpack_dict d)
      | _ -> iraise WrongPrimitiveArgs) in
  EvtDict (isvalidkey (k, v) :: (Dict.delete k d))


(** Remove a key-value pair from a dictionary *)
let delete_dict args =
  let (key, ed) = (match args with
      | [key; d] -> (key, unpack_dict d)
      | _ -> iraise WrongPrimitiveArgs) in
  if not (Dict.exists key ed) then iraise (DictError "key not found") else
    EvtDict (Dict.delete key ed)

(** Check if a key-value pair is in a dictionary *)
let haskey args =
  let (key, ed) = (match args with
      | [key; d] -> (key, unpack_dict d)
      | _ -> iraise WrongPrimitiveArgs) in
  EvtBool(Dict.exists key ed)

(** Check if a dict contains a key *)
let getkey args =
  let (key, ed) = (match args with
      | [key; d] -> (key, unpack_dict d)
      | _ -> iraise WrongPrimitiveArgs) in
  if not (Dict.exists key ed) then iraise (DictError "key not found") else
    Dict.get key ed


(** Check if a dict contains a key *)
let filterkeys args =
  let (kll, ed) = (match args with
      | [kl; d] -> (unpack_list kl, unpack_dict d)
      | _ -> iraise WrongPrimitiveArgs) in
  EvtDict(Dict.filter kll ed)

let table = [
  ("insert", (insert_dict, 3));
  ("remove", (delete_dict, 2));
  ("haskey", (haskey, 2));
  ("getkey", (getkey, 2));
  ("filterkeys", (filterkeys, 2))
]
