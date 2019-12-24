open Types
open Typecheck
open Util

let head act_params =
  if List.length act_params > 1 then raise (TooManyArgs "head") else
  let ls = unpack_list (List.hd act_params) in
  (match ls with
    | [] -> raise (ListError "empty list")
    | v::_ -> v )

let tail act_params =
  if List.length act_params > 1 then raise (TooManyArgs "tail") else
  let ls = unpack_list (List.hd act_params) in
  (match ls with
    | [] -> raise (ListError "empty list")
    | _::r -> EvtList r)

(** Insert a key-value pair in a dictionary *)
let insert_dict act_params =
  let (k, v, d) = (match act_params with
    | [k; v; d] -> (k, v, d)
    | _ -> raise WrongBindList) in
  EvtDict (isvalidkey (k, v) :: (unpack_dict d))

(** Remove a key-value pair from a dictionary *)
let delete_dict act_params  =
  let (key, d) = (match act_params with
    | [key; d] -> (key, d)
    | _ -> raise WrongPrimitiveArgs) in
  let ed = unpack_dict d in
  if not (key_exist key ed) then raise (DictError "key not found") else
  EvtDict (delete_key key ed)

(** Check if a key-value pair is in a dictionary *)
let haskey act_params =
  let (key, d) = (match act_params with
    | [key; d] -> (key, d)
    | _ -> raise WrongPrimitiveArgs) in
  EvtBool(key_exist key (unpack_dict d))

(** Check if a dict contains a key *)
let getkey act_params =
  let (key, d) = (match act_params with
    | [key; d] -> (key, d)
    | _ -> raise WrongPrimitiveArgs) in
  let ed = unpack_dict d in
  if not (key_exist key ed) then raise (DictError "key not found") else
  get_key_val key ed


let primitives_table = [
  ("insert", insert_dict);
  ("delete", delete_dict);
  ("haskey", haskey);
  ("getkey", getkey)
]