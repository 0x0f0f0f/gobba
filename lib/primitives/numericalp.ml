open Types
open Errors
open Typecheck
open Complex

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

let add args =
  let found, numlist = flatten_numbert_list args in
  match found with
  | TInt -> EvtInt (List.fold_left (+) 0 (unpackIntList numlist))
  | TFloat -> EvtFloat (List.fold_left (+.) 0. (unpackFloatList numlist))
  | TComplex -> EvtComplex (List.fold_left (Complex.add) Complex.zero
  (unpackComplexList numlist))
  | _ -> traise ("expected a value of type: number, found a value of type: " ^ (show_typeinfo found ))

let mult args =
  let found, numlist = flatten_numbert_list args in
  match found with
  | TInt -> EvtInt (List.fold_left ( * ) 1 (unpackIntList numlist))
  | TFloat -> EvtFloat (List.fold_left ( *. ) 1. (unpackFloatList numlist))
  | TComplex -> EvtComplex (List.fold_left (Complex.mul) {re = 1.; im = 1.}
  (unpackComplexList numlist))
  | _ -> traise ("expected a value of type: number, found a value of type: " ^ (show_typeinfo found ))


let sub args =
  let found, numlist = flatten_numbert_list args in
  let x, y = (match numlist with
    | [x; y] -> (x, y)
    | _ -> iraise (WrongPrimitiveArgs)) in
  match found with
  | TInt -> EvtInt (unpack_int x - unpack_int y)
  | TFloat -> EvtFloat (unpack_float x -. unpack_float y)
  | TComplex -> EvtComplex (Complex.sub (unpack_complex x) (unpack_complex y))
  | _ -> traise ("expected a value of type: number, found a value of type: " ^ (show_typeinfo found ))

let div args =
  let found, numlist = flatten_numbert_list args in
  let x, y = (match numlist with
    | [x; y] -> (x, y)
    | _ -> iraise (WrongPrimitiveArgs)) in
  match found with
  | TInt -> (* TOWRITE *)
    let xx = unpack_int x and yy = unpack_int y in
    if xx mod yy = 0 then EvtInt (xx / yy)
    else EvtFloat (float_of_int (unpack_int x) /. float_of_int (unpack_int y))
  | TFloat -> EvtFloat (unpack_float x /. unpack_float y)
  | TComplex -> EvtComplex (Complex.div (unpack_complex x) (unpack_complex y))
  | _ -> traise ("expected a value of type: number, found a value of type: " ^ (show_typeinfo found ))

let flatnum = (fun x -> flatten_numbert_list x |> snd |> fun y -> EvtList y)

let table = [
  ("flatnum", Primitive (flatnum, ("flatnum", 0, Pure)));
  ("add", Primitive (add, ("add", 0, Numerical)));
  ("mult", Primitive (mult, ("mult", 0, Numerical)));
  ("sub", Primitive (sub, ("sub", 2, Numerical)));
  ("div", Primitive (div, ("div", 2, Numerical)))
]
