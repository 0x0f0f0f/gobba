open Types
open Typecheck

let head args  =
  if List.length args > 1 then raise WrongPrimitiveArgs else
    let ls= unpack_list (List.hd args) in
    (match ls with
     | [] -> raise (ListError "empty list")
     | v::_ -> v )

let tail args =
  if List.length args > 1 then raise WrongPrimitiveArgs else
    let ls= unpack_list (List.hd args) in
    (match ls with
     | [] -> raise (ListError "empty list")
     | _::r -> EvtList r)

let mem args =
  let (elem, ls)= (match args with
      | [elem; ls] -> (elem, unpack_list ls)
      | _ -> raise WrongPrimitiveArgs) in
  EvtBool(List.mem elem ls)

let table = [
  ("head", (head, 1));
  ("tail", (tail, 1));
  ("mem", (mem, 2))
]

let js = {|
const head = R.head;
const tail = R.tail;
const mem = R.includes;
|}