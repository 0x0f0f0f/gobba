open Types
open Typecheck
(* open Util *)

(** String Primitives *)

let typeofp args =
  if List.length args != 1 then iraise WrongPrimitiveArgs else
  EvtString (show_tinfo (typeof (List.hd args)))

let table = [
  ("typeof", (typeofp, 1, Pure));
]

