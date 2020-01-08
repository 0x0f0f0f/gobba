open Types
open Complex
(* open Typecheck
open Util *)
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
  List.map (numLower found) l
