open Types

let w table = List.map (fun (k, v) -> (k, LazyExpression (lambda_from_primitive v))) table

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