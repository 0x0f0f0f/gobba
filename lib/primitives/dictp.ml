open Types
open Typecheck
open Util

(** Insert a key-value pair in a dictionary *)
let insert_dict act_params =
  let (k, v, d) = (match act_params with
    | [k; v; d] -> (k, v, unpack_dict d)
    | _ -> raise WrongBindList) in
  EvtDict (isvalidkey (k, v) :: (Dict.delete k d))


(** Remove a key-value pair from a dictionary *)
let delete_dict act_params  =
  let (key, ed) = (match act_params with
    | [key; d] -> (key, unpack_dict d)
    | _ -> raise WrongPrimitiveArgs) in
  if not (Dict.exists key ed) then raise (DictError "key not found") else
  EvtDict (Dict.delete key ed)

(** Check if a key-value pair is in a dictionary *)
let haskey act_params =
  let (key, ed) = (match act_params with
    | [key; d] -> (key, unpack_dict d)
    | _ -> raise WrongPrimitiveArgs) in
  EvtBool(Dict.exists key ed)

(** Check if a dict contains a key *)
let getkey act_params =
  let (key, ed) = (match act_params with
    | [key; d] -> (key, unpack_dict d)
    | _ -> raise WrongPrimitiveArgs) in
  if not (Dict.exists key ed) then raise (DictError "key not found") else
  Dict.get key ed


(** Check if a dict contains a key *)
let filterkeys act_params =
  let (kll, ed) = (match act_params with
    | [kl; d] -> (unpack_list kl, unpack_dict d)
    | _ -> raise WrongPrimitiveArgs) in
  EvtDict(Dict.filter kll ed)

let table = [
  ("insert", (insert_dict, 3));
  ("delete", (delete_dict, 2));
  ("haskey", (haskey, 2));
  ("getkey", (getkey, 2));
  ("filterkeys", (filterkeys, 2))
]