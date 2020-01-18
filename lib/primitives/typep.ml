open Types
open Errors
open Typecheck
(* open Util *)

(** String Primitives *)

let typeofp args =
  if List.length args != 1 then iraise WrongPrimitiveArgs else
  EvtString (show_typeinfo (typeof (List.hd args)))

let myfailwith args =
  if List.length args != 1 then iraise WrongPrimitiveArgs else
  let msg = unpack_string (List.hd args) in
  let _ = iraise (InternalFailure msg) in EvtUnit

let show args =
  match args with
  | [EvtString x] -> (EvtString x)
  | [x] -> EvtString (Values.show_unpacked_evt x)
  | _ -> iraise WrongPrimitiveArgs

let table = [
  ("show",              Primitive (show, ("show", 1, Pure)));
  ("string_from_value", Primitive (show, ("show", 1, Pure)));
  ("typeof", Primitive (typeofp, ("typeof", 1, Pure)));
  ("failwith", Primitive (myfailwith, ("failwith", 1, Pure)));
]

