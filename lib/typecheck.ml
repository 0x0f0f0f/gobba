open Types
open Util
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
  | Closure (_, _, _, p) -> TLambda p
  | RecClosure (_, _, _, _,p) -> TLambda p

(* Check if first elem of tuple is an allowed type for dict key and return tuple *)
let isvalidkey (x, y) = ((match x with
    | EvtInt _ -> x
    | EvtBool _ -> x
    | EvtString _ -> x
    | _ -> traise "value not allowed as dictionary key"), y)

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

(** "level out" the purity of two values *)
let level_purity a b = match (a, b) with
  | Impure, _ | _, Impure             -> Impure
  | Pure, Numerical | Numerical, Pure -> Pure
  | Numerical, Numerical              -> Numerical
  | Uncertain, _ | _, Uncertain       -> Uncertain
  | Pure, Pure                        -> Pure


(** Infer the purity of an expression. Note: this is a naive approach.
  This function is an abstract interpretation of expressions over primitives and environments.
  @param pt The primitives table
  @param paraml If inside a lambda, the list of parameters
  @param env An environment, needed to infer whether lazy expressions are pure or not *)
let rec infer_purity e pt paraml env =
  let inferp x = infer_purity x pt paraml env in
  let inferp2 a b =
    let ap = inferp a and bp = inferp b in
    level_purity ap bp in
  let inferpl ls =
    let apl = List.map inferp ls in
    List.fold_left level_purity Numerical apl in
  match e with
  | NumInt _ | NumFloat _ | NumComplex _ -> Numerical
  | Purity (Impure, _) -> Impure
  | Sequence l -> inferpl l
  (* Infer from all the binary operators *)
  | Compose (a, b) | Plus (a, b) | Sub (a, b) | Apply (a, b)
  | Mult (a, b) | Div (a, b) -> inferp2 a b
  | Lambda(p, b) -> infer_purity b pt (p::paraml) env
  | Let(assignments, b)
  | Letlazy (assignments, b) ->
    let identifiers = fstl assignments in
    let new_env =
      Dict.insertmany env identifiers
      (List.map (fun (_, value) -> LazyExpression value) assignments) in
    infer_purity b pt paraml new_env
  | Letrec(_, v, b) ->
    let ap = inferp v and bp = infer_purity b pt paraml env in
    level_purity ap bp
  | Symbol s ->
    if Dict.exists s pt then get_primitive_purity (Dict.get s pt)
    else if List.mem s paraml then Numerical
    else if Dict.exists s env then (match (Dict.get s env) with
      | LazyExpression ee -> inferp ee
      | AlreadyEvaluated (v) -> evt_purity v)
    else Pure
  | _ -> Pure

(** Obtain the purity level from an already evaluated value *)
and evt_purity e =
  match e with
  | EvtInt _| EvtFloat _ | EvtComplex _ -> Numerical
  | Closure(_, _, _, p) -> p
  | RecClosure(_, _, _, _, p) -> p
  | _ -> Pure

(* Static typechecking *)
let stcheck (f: typeinfo) (e: typeinfo) =
  let rterr () = terr (show_tinfo e) (show_tinfo f) in
  match e with
  | TNumber -> (match f with
      | TInt | TFloat | TComplex | TNumber -> ()
      | _ -> rterr () )
  | TLambda ep -> (match f with
      | TLambda fp -> if level_purity ep fp = ep then () else rterr()
      | _ -> rterr () )
  | _ -> if e = f then () else rterr()


(** Static typechecking inferer *)
let rec sinfer (e: expr) : typeinfo = match e with
  (* Inference only on TNumber. Rely on strict checking for precise number type checking *)
  | NumInt _
  | NumFloat _
  | NumComplex _ -> TNumber
  | Boolean _ -> TBool
  | String _ -> TString
  | List _ -> TList
  | Purity (_, x) -> sinfer x
  | Cons (_, b) ->
    (stcheck (sinfer b) TList);
    TList
  | ConcatLists (a, b) ->
    (stcheck (sinfer a) TList);
    (stcheck (sinfer b) TList);
    TList
  | Plus(a, b) ->
    (stcheck (sinfer a) TNumber);
    (stcheck (sinfer b) TNumber);
    TNumber
  | Sub(a, b) ->
    (stcheck (sinfer a) TNumber);
    (stcheck (sinfer b) TNumber);
    TNumber
  | Mult(a, b) -> (stcheck (sinfer a) (sinfer b));
    (stcheck (sinfer a) TNumber);
    (stcheck (sinfer b) TNumber);
    TNumber
  | Div(a, b) ->
    (stcheck (sinfer a) TNumber);
    (stcheck (sinfer b) TNumber);
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
let unpack_closure x = (match x with Closure (p, b, e, pu) -> (p,b,e,pu) | e -> terr "fun" (show_tinfo (typeof e)))
let unpack_recclosure x = (match x with
    | RecClosure (i, p, b, e, pu) -> (i,p,b,e,pu)
    | e -> terr "fun" (show_tinfo (typeof e)))
let unpack_anyfun x = match x with
  | RecClosure (i, p, b, e, pu) -> (i,p,b,e,pu)
  | Closure (p, b, e, pu) -> ("",p,b,e, pu)
  | e -> terr "fun" (show_tinfo (typeof e))