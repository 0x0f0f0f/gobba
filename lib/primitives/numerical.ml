open Types
open Typecheck
open Complex

type lowest_found = FInt | FFloat | FComplex

let rec find_lowest_kind low ls = match ls with
  | [] -> low
  | (EvtComplex _)::_ -> FComplex
  | (EvtInt _)::xs -> find_lowest_kind low xs
  | (EvtFloat _)::xs -> find_lowest_kind FFloat xs
  | (_)::_ -> raise (TypeError "value is not a number in arithmetical operator")

let numLower lowerto num = match lowerto with
  | FInt -> num
  | FFloat -> (match num with
    | EvtInt x -> EvtFloat(float_of_int x)
    | EvtFloat x -> EvtFloat x
    | EvtComplex x -> EvtComplex x
    | _ -> raise (TypeError "not a number"))
  | FComplex -> (match num with
    | EvtInt x -> EvtComplex {re = float_of_int x; im = 0.}
    | EvtFloat x -> EvtComplex {re = x; im = 0.}
    | EvtComplex x -> EvtComplex x
    | _ -> raise (TypeError "not a number"))

(** Accept a list of numbers and flatten out their
kind on the numerical tower hierarchy *)
let flattenNumList l =
  let found = find_lowest_kind FInt l in
  (found, List.map (numLower found) l)

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
  let found, numlist = flattenNumList args in
  match found with
  | FInt -> EvtInt (List.fold_left (+) 0 (unpackIntList numlist))
  | FFloat -> EvtFloat (List.fold_left (+.) 0. (unpackFloatList numlist))
  | FComplex -> EvtComplex (List.fold_left (Complex.add) Complex.zero
  (unpackComplexList numlist))

let mult args =
  let found, numlist = flattenNumList args in
  match found with
  | FInt -> EvtInt (List.fold_left ( * ) 1 (unpackIntList numlist))
  | FFloat -> EvtFloat (List.fold_left ( *. ) 1. (unpackFloatList numlist))
  | FComplex -> EvtComplex (List.fold_left (Complex.mul) {re = 1.; im = 1.}
  (unpackComplexList numlist))


let sub args =
  let found, numlist = flattenNumList args in
  let x, y = (match numlist with
    | [x; y] -> (x, y)
    | _ -> raise (WrongPrimitiveArgs)) in
  match found with
  | FInt -> EvtInt (unpack_int x - unpack_int y)
  | FFloat -> EvtFloat (unpack_float x -. unpack_float y)
  | FComplex -> EvtComplex (Complex.sub (unpack_complex x) (unpack_complex y))

let div args =
  let found, numlist = flattenNumList args in
  let x, y = (match numlist with
    | [x; y] -> (x, y)
    | _ -> raise (WrongPrimitiveArgs)) in
  match found with
  | FInt -> EvtInt (unpack_int x / unpack_int y)
  | FFloat -> EvtFloat (unpack_float x /. unpack_float y)
  | FComplex -> EvtComplex (Complex.div (unpack_complex x) (unpack_complex y))


let table = [
  ("flatnum", ((fun x -> flattenNumList x |> snd |> fun y -> EvtList y), 0));
  ("add", (add, 0)); ("sub", (sub, 2)); ("div", (div, 0)); ("mult", (mult, 0))
]
