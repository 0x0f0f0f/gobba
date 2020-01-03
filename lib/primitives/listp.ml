open Types
open Typecheck

let head args  =
  if List.length args > 1 then raise (TooManyArgs "head") else
    let ls= unpack_list (List.hd args) in
    (match ls with
     | [] -> raise (ListError "empty list")
     | v::_ -> v )

let tail args =
  if List.length args > 1 then raise (TooManyArgs "tail") else
    let ls= unpack_list (List.hd args) in
    (match ls with
     | [] -> raise (ListError "empty list")
     | _::r -> EvtList r)

let mem args =
  let (elem, ls)= (match args with
      | [elem; ls] -> (elem, unpack_list ls)
      | _ -> raise WrongBindList) in
  EvtBool(List.mem elem ls)

let table = [
  ("head", (head, 1));
  ("tail", (tail, 1));
  ("mem", (mem, 2))
]

let js = {|
  function head (list) { return list[0]; }
  function tail (list) { return list.slice(1) }
  function mem (elem, list) { return list.includes(elem); }
|}