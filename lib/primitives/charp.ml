open Types
open Errors
open Typecheck

let toupper args =
  let x = match args with
    | [x] -> unpack_char x
    | _ -> iraise WrongPrimitiveArgs in
  EvtChar(Char.uppercase_ascii x)

let table = [
  ("upcase", Primitive (toupper, ("upcase", 1, Pure)));
]

