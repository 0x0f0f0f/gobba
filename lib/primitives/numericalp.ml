open Types
open Errors
open Typecheck
open Owl_base_maths

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

let first_two_numbers (args: evt array) =
  if Array.length args <> 2 then iraise WrongPrimitiveArgs else
  let found, numlist = flatten_numbert_list (Array.to_list args) in
  (match numlist with
    | [x; y] -> (found, x, y)
    | _ -> iraise (WrongPrimitiveArgs))

let dynamic_binop 
  (opint : int -> int -> int)
  (opfloat : float -> float -> float)
  (opcomplex : Complex.t -> Complex.t -> Complex.t)
  args = 
  let t, x, y = first_two_numbers args in
  match t with
  | TInt -> EvtInt (opint (unpack_int x) (unpack_int y))
  | TFloat -> EvtFloat (opfloat (unpack_float x) (unpack_float y))
  | TComplex -> EvtComplex (opcomplex (unpack_complex x) (unpack_complex y))
  | _ -> traise ("expected a value of type: number, found a value of type: " ^ (show_typeinfo t ))


let add args = dynamic_binop (+) (+.) Complex.add args
let mult args = dynamic_binop ( * ) ( *. ) Complex.mul args 
let sub args = dynamic_binop (-) (-.) Complex.sub args

let div args =
  let t, x, y = first_two_numbers args in
  match t with
  | TInt -> (* TOWRITE *)
    let xx = unpack_int x and yy = unpack_int y in
    if xx mod yy = 0 then EvtInt (xx / yy)
    else EvtFloat (float_of_int (unpack_int x) /. float_of_int (unpack_int y))
  | TFloat -> EvtFloat (unpack_float x /. unpack_float y)
  | TComplex -> EvtComplex (Complex.div (unpack_complex x) (unpack_complex y))
  | _ -> traise ("expected a value of type: number, found a value of type: " ^ (show_typeinfo t ))

let makecomplex args =
  let _, x, y = first_two_numbers args in
  match x, y with
  | EvtInt xx, EvtInt yy -> EvtComplex {Complex.re = float_of_int xx; Complex.im = float_of_int yy}
  | EvtFloat xx, EvtFloat yy -> EvtComplex {Complex.re = xx; Complex.im = yy}
  | EvtComplex _, _ -> traise "A component of a complex number cannot be another complex number"
  | _ -> traise "Internal type inconsistency when allocating a complex number"

let flatnum args =
  flatten_numbert_list (Array.to_list args) |> snd |> fun y -> EvtList y

(* Numerical primitive wrappers *)
let int_unop (op: int -> int) args =
  let x = (match args with
  | [|x|] -> unpack_int @@ cast_numbert TFloat x
  | _ -> iraise WrongPrimitiveArgs ) in
  EvtInt(op x)

let float_unop (op: float -> float) args =
  let x = (match args with
  | [|x|] -> unpack_float @@ cast_numbert TFloat x
  | _ -> iraise WrongPrimitiveArgs ) in
  EvtFloat(op x)

let float_binop (op: float -> float -> float) args =
  let x, y = (match args with
  | [|x; y|] -> (unpack_float @@ cast_numbert TFloat x), unpack_float @@ cast_numbert TFloat y
  | _ -> iraise WrongPrimitiveArgs ) in
  EvtFloat(op x y)

let table = [
  (* Exponentiation and Roots *)
  ("exp", Primitive (float_unop exp, ("exp", [|"number"|], Numerical)));
  ("pow", Primitive (float_binop pow, ("pow", [|"number"|], Numerical)));
  ("sqrt", Primitive (float_unop sqrt, ("sqrt", [|"number"|], Numerical)));
  ("cbrt", Primitive (float_unop cbrt, ("cbrt", [|"number"|], Numerical)));
  (* Trigonometry *)
  ("cos", Primitive (float_unop cos, ("cos", [|"number"|], Numerical)));
  ("sin", Primitive (float_unop sin, ("sin", [|"number"|], Numerical)));
  ("tan", Primitive (float_unop tan, ("tan", [|"number"|], Numerical)));
  ("cot", Primitive (float_unop cot, ("cot", [|"number"|], Numerical)));
  ("acos", Primitive (float_unop acos, ("acos", [|"number"|], Numerical)));
  ("asin", Primitive (float_unop asin, ("asin", [|"number"|], Numerical)));
  ("atan", Primitive (float_unop atan, ("atan", [|"number"|], Numerical)));
  ("acot", Primitive (float_unop acot, ("acot", [|"number"|], Numerical)));
  ("cosh", Primitive (float_unop cosh, ("cosh", [|"number"|], Numerical)));
  ("sinh", Primitive (float_unop sinh, ("sinh", [|"number"|], Numerical)));
  ("tanh", Primitive (float_unop tanh, ("tanh", [|"number"|], Numerical)));
  ("acosh", Primitive (float_unop acosh, ("acosh", [|"number"|], Numerical)));
  ("asinh", Primitive (float_unop asinh, ("asinh", [|"number"|], Numerical)));
  ("atanh", Primitive (float_unop atanh, ("atanh", [|"number"|], Numerical)));
  (* Error function *)
  ("erf", Primitive (float_unop erf, ("erf", [|"number"|], Numerical)));  
  (* Misc *)
  (* ("is_prime", Primitive (int_unop is_prime, ("is_prime", [|"number"|], Numerical))); *)
  
]

let constants = [
  ("e", EvtFloat(exp 1.));
  ("pi", EvtFloat(3.1415926535897932384626433));
  ("nan", EvtFloat(nan))
]