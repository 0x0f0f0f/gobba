open Types
open Typecheck

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

let table = [
  ("head", head);
  ("tail", tail)
]