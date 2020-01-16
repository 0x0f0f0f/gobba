open Types
open Errors

let terr e f = traise ("expected a value of type: " ^ e ^ ", found a value of type: " ^ f )

let typeof e = match e with
  | EvtUnit -> TUnit
  | EvtInt _ -> TInt
  | EvtFloat _ -> TFloat
  | EvtComplex _ -> TComplex
  | EvtBool _ -> TBool
  | EvtString _ -> TString
  | EvtList _ -> TList
  | EvtDict _ -> TDict
  | Closure (_, _, _, _) -> TLambda
  | LazyExpression _ -> TUnit

(** Get the lowest (most inclusive set) number type from a list of numbers *)
let rec infer_lowest_numbert low ls = match ls with
  | [] -> low
  | (EvtComplex _)::_ -> TComplex
  | (EvtInt _)::xs -> infer_lowest_numbert low xs
  | (EvtFloat _)::xs -> infer_lowest_numbert TFloat xs
  | (_)::_ -> traise "value is not a number in arithmetical operator"

let cast_numbert lowerto num = match lowerto with
  | TInt -> num
  | TFloat -> (match num with
      | EvtInt x -> EvtFloat(float_of_int x)
      | EvtFloat x -> EvtFloat x
      | EvtComplex x -> EvtComplex x
      | _ -> traise "not a number")
  | TComplex -> (match num with
      | EvtInt x -> EvtComplex {re = float_of_int x; im = 0.}
      | EvtFloat x -> EvtComplex {re = x; im = 0.}
      | EvtComplex x -> EvtComplex x
      | _ -> traise "not a number")
  | _ -> traise "cannot cast to a non-numerical type"

(** Accept a list of numbers and flatten out their
    kind on the numerical tower hierarchy *)
let flatten_numbert_list l =
  let found = infer_lowest_numbert TInt l in
  (found, List.map (cast_numbert found) l)


(* Static typechecking *)
let stcheck (f: typeinfo) (e: typeinfo) =
  let rterr () = terr (show_tinfo e) (show_tinfo f) in
  match e with
  | TNumber -> (match f with
      | TInt | TFloat | TComplex | TNumber -> ()
      | _ -> rterr () )
  | _ -> if e = f then () else rterr()


(** Static typechecking inferer *)
let rec sinfer (e: expr) (state: evalstate) : typeinfo = match e with
  (* Inference only on TNumber. Rely on strict checking for precise number type checking *)
  | NumInt _ | NumFloat _ | NumComplex _ -> TNumber
  | Boolean _ -> TBool
  | String _ -> TString
  | List _ -> TList
  | Purity (_, x) -> sinfer x state
  | Binop(Cons, _, b) -> (stcheck (sinfer b state) TList); TList
  | Binop(Concat, a, b) ->
    let ta = sinfer a state and tb = sinfer b state in
    (match (ta, tb) with
        | TString, TString -> TString
        | TList, TList -> TList
        | _ -> iraises (TypeError (Printf.sprintf "Cannot concatenate a two values of type %s and %s"
          (show_tinfo ta) (show_tinfo tb))) state.stack )
  | Binop(Plus, a, b) | Binop(Sub, a, b)
  | Binop(Mult, a, b) | Binop(Div, a, b) ->
    (stcheck (sinfer a state) TNumber);
    (stcheck (sinfer b state) TNumber);
    TNumber
  | _ -> traise "Could not infer type!"

(** Unpacking functions: extract a value or throw an err *)

let unpack_int x = (match x with EvtInt i -> i | e -> terr "int" (show_tinfo (typeof e)))
let unpack_float x = (match x with EvtFloat i -> i | e -> terr "float" (show_tinfo (typeof e)))
let unpack_complex x = (match x with EvtComplex i -> i | e -> terr "complex" (show_tinfo (typeof e)))
let unpack_bool x = (match x with EvtBool i -> i | e -> terr "bool" (show_tinfo (typeof e)))
let unpack_string x = (match x with EvtString i -> i | e -> terr "string" (show_tinfo (typeof e)))
let unpack_list x = (match x with EvtList i -> i | e -> terr "list" (show_tinfo (typeof e)))
let unpack_dict x = (match x with EvtDict i -> i | e -> terr "dict" (show_tinfo (typeof e)))
let unpack_closure x = (match x with Closure (n, p, b, e) -> (n, p,b,e) | e -> terr "fun" (show_tinfo (typeof e)))
