open Types
open Errors
open Typecheck

let toupper args =
  let x = match args with
    | [|x|] -> unpack_char x
    | _ -> iraise WrongPrimitiveArgs in
  EvtChar(Char.uppercase_ascii x)

let tolower args =
  let x = match args with
    | [|x|] -> unpack_char x
    | _ -> iraise WrongPrimitiveArgs in
  EvtChar(Char.lowercase_ascii x)


let table = [
  ("toupper", Primitive (toupper, ("toupper", [|"char"|], Pure)));
  ("tolower", Primitive (tolower, ("tolower", [|"char"|], Pure)));
]

