(** This module includes String Primitives *)

open Types
open Errors
open Typecheck
open Primutil
(* open Util *)


let compare (comp: (int -> int -> bool)) args =
  let (x,y) = match args with
    | [|x;y|] -> (x, y)
    | _ -> iraise WrongPrimitiveArgs in
  EvtBool(comp (compare_evt x y) 0)

let concat args = match args with
  | [|x;y|] -> EvtString ((unpack_string x) ^ (unpack_string y))
  | _ -> iraise WrongPrimitiveArgs

let explode args =
  let x = match args with
  | [|x|] -> unpack_string x
  | _ -> iraise WrongPrimitiveArgs in
  EvtList(Util.explode x |> List.map (fun x -> EvtChar x))

let implode args =
  let x = match args with
  | [|x|] -> let ls = unpack_list x in
    List.map unpack_char ls
  | _ -> iraise WrongPrimitiveArgs in
  EvtString(Util.implode x)


let mapstr = {|
fun f str -> let expl = String:explode str in
  let mapped = List:map f expl in
  String:implode mapped
|}

let lambda_table = [
  ("map", (lambda_of_string "map" mapstr))
]


let table = [
  ("concat", Primitive (concat, ("concat", [|"a"; "b"|], Pure)));
  ("explode", Primitive (explode, ("explode", [|"string"|], Pure)));
  ("implode", Primitive (implode, ("implode", [|"string"|], Pure)))
]

