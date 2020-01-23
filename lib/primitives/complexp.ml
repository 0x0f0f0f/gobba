open Types
open Errors
open Typecheck

let realpart args =
  let x = match args with
    | [|x|] -> unpack_complex x
    | _ -> iraise WrongPrimitiveArgs in
  EvtFloat(x.re)

let imagpart args =
  let x = match args with
    | [|x|] -> unpack_complex x
    | _ -> iraise WrongPrimitiveArgs in
  EvtFloat(x.im)

let conj args =
  let x = match args with
    | [|x|] -> unpack_complex x
    | _ -> iraise WrongPrimitiveArgs in
  EvtComplex(Complex.conj x)

let inv args =
  let x = match args with
    | [|x|] -> unpack_complex x
    | _ -> iraise WrongPrimitiveArgs in
  EvtComplex(Complex.inv x)

let of_polar args =
  let x, y = match args with
    | [|x; y|] -> unpack_float x, unpack_float y;
    | _ -> iraise WrongPrimitiveArgs in
  EvtComplex(Complex.polar x y)


let table = [
  ("real", Primitive (realpart, ("real", [|"num"|], Numerical)));
  ("imag", Primitive (imagpart, ("imag", [|"num"|], Numerical)));
  ("conj", Primitive (conj, ("conj", [|"num"|], Numerical)));
  ("inv", Primitive (inv, ("inv", [|"num"|], Numerical)));
]
