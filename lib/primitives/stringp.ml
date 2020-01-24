open Types
open Errors
open Typecheck
(* open Util *)

(** String Primitives *)

let compare (comp: (int -> int -> bool)) args =
  let (x,y) = match args with
    | [|x;y|] -> (x, y)
    | _ -> iraise WrongPrimitiveArgs in
  EvtBool(comp (compare_evt x y) 0)

let concat args = match args with
  | [|x;y|] -> EvtString ((unpack_string x) ^ (unpack_string y))
  | _ -> iraise WrongPrimitiveArgs

let table = [
  ("concat", Primitive (concat, ("concat", [|"a"; "b"|], Pure)));
]

