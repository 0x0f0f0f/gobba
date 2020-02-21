open Types
open Errors

let pprint_endline args =
  let x = (match args with
  | [|EvtString x|] -> x
  | [|x|] -> Values.show_evt_fancy x
  | _ -> iraise WrongPrimitiveArgs) in
  Printf.printf "%s\n%!" x; EvtUnit

let pprint args =
  let x = (match args with
  | [|EvtString x|] -> x
  | [|x|] -> Values.show_evt_fancy x
  | _ -> iraise WrongPrimitiveArgs) in
  Printf.printf "%s%!" x; EvtUnit

let exitp args =
  let code = match args with
  | [|EvtInt x|] -> x
  | _ -> iraise WrongPrimitiveArgs in
  exit code

let table = [
  ("exit",         Primitive (exitp, ("exit", [|"code"|], Pure)));
  ("print",         Primitive (pprint, ("print", [|"anything"|], Impure)));
  ("print_endline", Primitive (pprint_endline, ("print_endline", [|"anything"|], Impure)));
]
