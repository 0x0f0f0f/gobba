open Types
open Errors

let pprint_endline args =
  let x = (match args with
  | [EvtString x] -> x
  | [x] -> Values.show_unpacked_evt x
  | _ -> iraise WrongPrimitiveArgs) in
  Printf.printf "%s\n%!" x; EvtUnit

let pprint args =
  let x = (match args with
  | [EvtString x] -> x
  | [x] -> Values.show_unpacked_evt x
  | _ -> iraise WrongPrimitiveArgs) in
  Printf.printf "%s%!" x; EvtUnit

let table = [
  ("print",         Primitive (pprint, ("print", 1, Impure)));
  ("print_endline", Primitive (pprint_endline, ("print_endline", 1, Impure)));
]
