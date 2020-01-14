open Types
open Typecheck
(* open Util *)

(** String Primitives *)

let string_binop (op : string -> string -> string) args =
  let (x, y) = match args with
    | [x; y] -> (unpack_string x, unpack_string y)
    | _ -> iraise WrongPrimitiveArgs in
  EvtString (op x y)

let string_unop (op : string -> string) args =
  let x = match args with
    | [x] -> unpack_string x
    | _ -> iraise WrongPrimitiveArgs in
  EvtString (op x)

let compare (comp: (int -> int -> bool)) args =
  let (x,y) = match args with
    | [x;y] -> (x, y)
    | _ -> iraise WrongPrimitiveArgs in
  EvtBool(comp (compare_evt x y) 0)

let concat args = match args with
  | [x;y] -> EvtString ((unpack_string x) ^ (unpack_string y))
  | _ -> iraise WrongPrimitiveArgs


let show args =
  match args with
  | [EvtString x] -> (EvtString x)
  | [x] -> EvtString (show_unpacked_evt x)
  | _ -> iraise WrongPrimitiveArgs

let table = [
  ("^",                 Primitive (concat, ("concat", 2, Pure)));
  ("concat",            Primitive (concat, ("concat", 2, Pure)));
  ("show",              Primitive (show, ("show", 1, Pure)));
  ("string_from_value", Primitive (show, ("show", 1, Pure)));
]

