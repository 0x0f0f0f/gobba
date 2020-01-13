open Types
open Typecheck
(* open Util *)

(** String Primitives *)

let typeofp args =
  if List.length args != 1 then iraise WrongPrimitiveArgs else
  EvtString (show_tinfo (typeof (List.hd args)))

let myfailwith args =
  if List.length args != 1 then iraise WrongPrimitiveArgs else
  let msg = unpack_string (List.hd args) in
  let _ = iraise (InternalFailure msg) in EvtUnit

let table = [
  ("typeof", (typeofp, 1, Pure));
  ("failwith", (myfailwith, 1, Pure));
]

