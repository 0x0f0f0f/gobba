open Types
open Errors
open Typecheck
(* open Util *)

(** String Primitives *)

let typeofp args =
  if Array.length args != 1 then iraise WrongPrimitiveArgs else
  EvtString (show_typeinfo (typeof args.(0)))

let myfailwith args =
  if Array.length args != 1 then iraise WrongPrimitiveArgs else
  let msg = unpack_string args.(0) in
  let _ = iraise (InternalFailure msg) in EvtUnit

let show args =
  match args with
  | [|EvtString x|] -> (EvtString x)
  | [|x|] -> EvtString (Values.show_unpacked_evt x)
  | _ -> iraise WrongPrimitiveArgs

let table = [
  ("show",              Primitive (show, ("show", [|"anything"|], Pure)));
  ("string_from_value", Primitive (show, ("show", [|"anything"|], Pure)));
  ("typeof", Primitive (typeofp, ("typeof", [|"anything"|], Pure)));
  ("failwith", Primitive (myfailwith, ("failwith", [|"message"|], Pure)));
]

