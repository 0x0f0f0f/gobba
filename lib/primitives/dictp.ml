open Types
open Typecheck
open Util

(** Insert a key-value pair in a dictionary *)
let insert_dict args =
  let (k, v, d) = (match args with
      | [k; v; d] -> (k, v, unpack_dict d)
      | _ -> raise WrongBindList) in
  EvtDict (isvalidkey (k, v) :: (Dict.delete k d))


(** Remove a key-value pair from a dictionary *)
let delete_dict args =
  let (key, ed) = (match args with
      | [key; d] -> (key, unpack_dict d)
      | _ -> raise WrongPrimitiveArgs) in
  if not (Dict.exists key ed) then raise (DictError "key not found") else
    EvtDict (Dict.delete key ed)

(** Check if a key-value pair is in a dictionary *)
let haskey args =
  let (key, ed) = (match args with
      | [key; d] -> (key, unpack_dict d)
      | _ -> raise WrongPrimitiveArgs) in
  EvtBool(Dict.exists key ed)

(** Check if a dict contains a key *)
let getkey args =
  let (key, ed) = (match args with
      | [key; d] -> (key, unpack_dict d)
      | _ -> raise WrongPrimitiveArgs) in
  if not (Dict.exists key ed) then raise (DictError "key not found") else
    Dict.get key ed


(** Check if a dict contains a key *)
let filterkeys args =
  let (kll, ed) = (match args with
      | [kl; d] -> (unpack_list kl, unpack_dict d)
      | _ -> raise WrongPrimitiveArgs) in
  EvtDict(Dict.filter kll ed)

let table = [
  ("insert", (insert_dict, 3));
  ("remove", (delete_dict, 2));
  ("haskey", (haskey, 2));
  ("getkey", (getkey, 2));
  ("filterkeys", (filterkeys, 2))
]

let js = {|
function insert (key, val, dict) {
  let __new = Object.assign({}, dict);
  __new[key] = val;
  return __new;
}
function remove (key, dict) {
  let result = {};
  for(k in dict) {
    if (k != key)
      { result[k] = dict[k]; }
  }
  return result;
}
function haskey (key, dict) { return (key in dict) }
function getkey (key, dict) { return dict[key] }
function filterkeys (keys, dict) { return (
  Object.keys(dict)
  .filter(key => keys.includes(key))
  .reduce((obj, key) => {
    obj[key] = dict[key];
    return obj;
  }, {}))}
|}