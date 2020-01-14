open Types

let pprint_endline args =
  let x = (match args with
  | [EvtString x] -> x
  | [x] -> show_unpacked_evt x
  | _ -> iraise WrongPrimitiveArgs) in
  print_endline x; EvtUnit

let pprint args =
  let x = (match args with
  | [EvtString x] -> x
  | [x] -> show_unpacked_evt x
  | _ -> iraise WrongPrimitiveArgs) in
  print_string x; EvtUnit

let table = [
  ("print",         Primitive (pprint, ("print", 1, Impure)));
  ("print_endline", Primitive (pprint_endline, ("print_endline", 1, Impure)));
]
