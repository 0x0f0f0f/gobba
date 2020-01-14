open Types

let ocaml_table = Numericalp.table @ Dictp.table
  @ Listp.table @ Stringp.table
  @ Typep.table @ Iop.table

let table: env_type = (List.map (fun (n, (_, v)) -> (n, v)) Mstdlib.table) @
  (List.map (fun (k, v) -> (k, LazyExpression (lambda_from_primitive v))) ocaml_table)

let purity_table: purityenv_type =
  (List.map (fun (n, (p, _)) -> (n, p)) Mstdlib.table) @
  (List.map (fun (k, v) -> (k, get_primitive_purity v)) ocaml_table)