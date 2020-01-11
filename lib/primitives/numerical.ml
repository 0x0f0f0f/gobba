open Types
open Typecheck
open Complex

let rec unpackIntList l = match l with
  | [] -> []
  | (EvtInt x) :: xs -> x::(unpackIntList xs)
  | _::_ ->  raise (TypeError "internal type error")

let rec unpackFloatList l = match l with
  | [] -> []
  | (EvtFloat x) :: xs -> x::(unpackFloatList xs)
  | _::_ ->  raise (TypeError "internal type error")

let rec unpackComplexList l = match l with
  | [] -> []
  | (EvtComplex x) :: xs -> x::(unpackComplexList xs)
  | _::_ ->  raise (TypeError "internal type error")

let add args =
  let found, numlist = flatten_numbert_list args in
  match found with
  | TInt -> EvtInt (List.fold_left (+) 0 (unpackIntList numlist))
  | TFloat -> EvtFloat (List.fold_left (+.) 0. (unpackFloatList numlist))
  | TComplex -> EvtComplex (List.fold_left (Complex.add) Complex.zero
  (unpackComplexList numlist))
  | _ -> raise (TypeError "not a number")

let mult args =
  let found, numlist = flatten_numbert_list args in
  match found with
  | TInt -> EvtInt (List.fold_left ( * ) 1 (unpackIntList numlist))
  | TFloat -> EvtFloat (List.fold_left ( *. ) 1. (unpackFloatList numlist))
  | TComplex -> EvtComplex (List.fold_left (Complex.mul) {re = 1.; im = 1.}
  (unpackComplexList numlist))
  | _ -> raise (TypeError "not a number")


let sub args =
  let found, numlist = flatten_numbert_list args in
  let x, y = (match numlist with
    | [x; y] -> (x, y)
    | _ -> raise (WrongPrimitiveArgs)) in
  match found with
  | TInt -> EvtInt (unpack_int x - unpack_int y)
  | TFloat -> EvtFloat (unpack_float x -. unpack_float y)
  | TComplex -> EvtComplex (Complex.sub (unpack_complex x) (unpack_complex y))
  | _ -> raise (TypeError "not a number")

let div args =
  let found, numlist = flatten_numbert_list args in
  let x, y = (match numlist with
    | [x; y] -> (x, y)
    | _ -> raise (WrongPrimitiveArgs)) in
  match found with
  | TInt -> EvtInt (unpack_int x / unpack_int y)
  | TFloat -> EvtFloat (unpack_float x /. unpack_float y)
  | TComplex -> EvtComplex (Complex.div (unpack_complex x) (unpack_complex y))
  | _ -> raise (TypeError "not a number")


let table = [
  ("flatnum", ((fun x -> flatten_numbert_list x |> snd |> fun y -> EvtList y), 0));
  ("add", (add, 0)); ("sub", (sub, 2)); ("div", (div, 0)); ("mult", (mult, 0))
]
