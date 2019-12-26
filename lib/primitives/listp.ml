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

let mem act_params =
  let (elem, ls) = (match act_params with
    | [elem; ls] -> (elem, unpack_list ls)
    | _ -> raise WrongBindList) in
  EvtBool(List.mem elem ls)

let table = [
  ("head", (head, 1));
  ("tail", (tail, 1));
  ("mem", (mem, 2))
]