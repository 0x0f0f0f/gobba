(** Basic Testing Primitives *)

open Types
open Errors
open Typecheck

(** Assert if two values are equal, otherwise fail *)
let assertp args =
  let (x,y) = match args with
    | [|x;y|] -> (x, y)
    | _ -> iraise WrongPrimitiveArgs in
  if (compare_evt x y) <> 0 then
    iraise @@ InternalFailure ("Assertion Error: expected " ^ (show_evt y) ^ " but got " ^ (show_evt x))
  else EvtUnit

let unit_assert args =
  let (desc, x,y) = match args with
    | [|desc; x;y|] -> (unpack_string desc, x, y)
    | _ -> iraise WrongPrimitiveArgs in
  if (compare_evt x y) <> 0 then
    (print_message
      ~color:T.Red
      ~loc:Nowhere
      "FAIL"
      desc; EvtBool false)
  else (print_message
      ~color:T.Green
      ~loc:Nowhere
      "PASS"
      desc; EvtBool true)


let table = [
  ("assert", Primitive (assertp, ("assert", [|"a"; "b"|], Pure)));
  ("unit_assert", Primitive (unit_assert, ("unit_assert", [|"description"; "a"; "b"|], Pure)));
]

