open Types
open Errors
open Typecheck

let rec unpackIntList l = match l with
  | [] -> []
  | (EvtInt x) :: xs -> x::(unpackIntList xs)
  | _::_ ->  traise "internal type error"

let rec unpackFloatList l = match l with
  | [] -> []
  | (EvtFloat x) :: xs -> x::(unpackFloatList xs)
  | _::_ ->  traise "internal type error"

let rec unpackComplexList l = match l with
  | [] -> []
  | (EvtComplex x) :: xs -> x::(unpackComplexList xs)
  | _::_ -> traise "internal type error"

let number_heads args =
  if List.length args <> 2 then iraise WrongPrimitiveArgs else
  let found, numlist = flatten_numbert_list args in
  (match numlist with
    | [x; y] -> (found, x, y)
    | _ -> iraise (WrongPrimitiveArgs))

let add args =
  let found, numlist = flatten_numbert_list args in
  match found with
  | TInt -> EvtInt (List.fold_left (+) 0 (unpackIntList numlist))
  | TFloat -> EvtFloat (List.fold_left (+.) 0. (unpackFloatList numlist))
  | TComplex -> EvtComplex (List.fold_left (Complex.add) Complex.zero
  (unpackComplexList numlist))
  | _ -> traise ("expected a value of type: number, found a value of type: " ^ (show_typeinfo found ))

let mult args =
  let t, x, y = number_heads args in
  match t with
  | TInt -> EvtInt (unpack_int x * unpack_int y)
  | TFloat -> EvtFloat (unpack_float x *. unpack_float y)
  | TComplex -> EvtComplex (Complex.mul (unpack_complex x) (unpack_complex y))
  | _ -> traise ("expected a value of type: number, found a value of type: " ^ (show_typeinfo t ))


let sub args =
  let t, x, y = number_heads args in
  match t with
  | TInt -> EvtInt (unpack_int x - unpack_int y)
  | TFloat -> EvtFloat (unpack_float x -. unpack_float y)
  | TComplex -> EvtComplex (Complex.sub (unpack_complex x) (unpack_complex y))
  | _ -> traise ("expected a value of type: number, found a value of type: " ^ (show_typeinfo t ))

let div args =
  let t, x, y = number_heads args in
  match t with
  | TInt -> (* TOWRITE *)
    let xx = unpack_int x and yy = unpack_int y in
    if xx mod yy = 0 then EvtInt (xx / yy)
    else EvtFloat (float_of_int (unpack_int x) /. float_of_int (unpack_int y))
  | TFloat -> EvtFloat (unpack_float x /. unpack_float y)
  | TComplex -> EvtComplex (Complex.div (unpack_complex x) (unpack_complex y))
  | _ -> traise ("expected a value of type: number, found a value of type: " ^ (show_typeinfo t ))

let makecomplex args =
  let _, x, y = number_heads args in
  match x, y with
  | EvtInt xx, EvtInt yy -> EvtComplex {Complex.re = float_of_int xx; Complex.im = float_of_int yy}
  | EvtFloat xx, EvtFloat yy -> EvtComplex {Complex.re = xx; Complex.im = yy}
  | EvtComplex _, _ -> traise "A component of a complex number cannot be another complex number"
  | _ -> traise "Internal type inconsistency when allocating a complex number"

let flatnum = (fun x -> flatten_numbert_list x |> snd |> fun y -> EvtList y)

let table = [
  ("flatnum", Primitive (flatnum, ("flatnum", 0, Pure)));
  ("complex_of_num", Primitive (div, ("complex_of_num", 2, Numerical)))
]
