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
    let name, numparams, purity = get_primitive_info prim in
    (* Generate a closure abstraction from a primitive *)
    let primargs = Util.generate_prim_params numparams in
    let symprimargs = Expr.symbols_from_strings primargs in
    let lambdas = Expr.lambda_of_paramlist primargs (ApplyPrimitive((name, numparams, purity), symprimargs)) in
    lambdas

let w table = List.map (fun (k, v) -> (k, LazyExpression (lambda_of_primitive v))) table

(* The plain table of primitive functions (key - name) *)
let ocaml_table =
  Numericalp.table @
  Dictp.table @
  Listp.table @
  Stringp.table @
  Typep.table @
  Iop.table

(* The table of primitives or primitive modules wrapped in an Evt *)
let wrapped_ocaml_table =
  (w Numericalp.table) @
  ["Dict", EvtDict (w Dictp.table)] @
  ["String", EvtDict (w Stringp.table)] @
  (w Listp.table) @
  (w Typep.table) @
  ["IO", EvtDict (w Iop.table)]

(* The table of primitives (textual and native) *)
let table: env_type = Mstdlib.table @ wrapped_ocaml_table


let rec purity_from_env (m: env_type) : purityenv_type = match m with
  | [] -> []
  | (modulename, EvtDict dict)::xs -> (modulename, PurityModule (purity_from_env dict)) :: (purity_from_env xs)
  | (pname, _)::xs ->
    let prim = (match (Util.Dict.get pname ocaml_table) with
      | Some p -> get_primitive_purity p
      | None -> failwith "Fatal Error: could not allocate primitive purity table") in
    (pname, prim) :: (purity_from_env xs)

  (* (pname, PurityValue (get_primitive_purity (Dict.get name ocaml_table))::(purity_from_env xs)
 *)
let purity_table: purityenv_type =
  Mstdlib.purity_table @ (purity_from_env wrapped_ocaml_table)