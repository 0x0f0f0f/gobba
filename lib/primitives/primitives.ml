open Types


(** Get the purity of a primitive *)
let get_primitive_purity x = match x with
  Primitive (_, (_, _, p)) -> p

(** Get the actual function from a primitive type *)
let get_primitive_function x = match x with
  Primitive (f, _) -> f

(** Get the information from a primitive type *)
let get_primitive_info x = match x with
  Primitive (_, i) -> i

(** Generate a lambda from a native primitive *)
let lambda_of_primitive prim =
    let name, params, purity = get_primitive_info prim in
    (* Generate a closure abstraction from a primitive *)
    let symparams = Array.map (fun x -> Symbol x) params in
    let lambdas = Expr.lambda_of_paramarr params (ApplyPrimitive((name, params, purity), symparams)) in
    lambdas

let w table = List.map (fun (k, v) -> (k, LazyExpression (lambda_of_primitive v))) table

(* The plain table of primitive functions (key - name) *)
let ocaml_table =
  Numericalp.table @
  Dictp.table @
  Listp.table @
  Charp.table @
  Stringp.table @
  Typep.table @
  Iop.table

(* The table of primitives or primitive modules wrapped in an Evt *)
let table: env_type =
  (w Numericalp.table) @
  (w Typep.table) @
  ["Dict", EvtDict ((w Dictp.table) @ Dictp.lambda_table)] @
  ["Char", EvtDict (w Charp.table)] @
  ["String", EvtDict (w Stringp.table)] @
  ["List", EvtDict ((w Listp.table) @ Listp.lambda_table)] @
  ["IO", EvtDict (w Iop.table)]


let rec purity_from_env (m: env_type) : purityenv_type = match m with
  | [] -> []
  | (modulename, EvtDict dict)::xs -> (modulename, PurityModule (purity_from_env dict)) :: (purity_from_env xs)
  | (pname, _)::xs ->
    let prim = (match (Util.Dict.get pname ocaml_table) with
      | Some p -> get_primitive_purity p
      | None -> Pure ) in
    (pname, prim) :: (purity_from_env xs)

  (* (pname, PurityValue (get_primitive_purity (Dict.get name ocaml_table))::(purity_from_env xs)
 *)
let purity_table: purityenv_type = (purity_from_env table)