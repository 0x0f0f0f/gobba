open Types

let w table = List.map (fun (k, v) -> (k, LazyExpression (lambda_from_primitive v))) table

let ocaml_table =
  Numericalp.table @
  Dictp.table @
  Listp.table @
  Stringp.table @
  Typep.table @
  Iop.table

let table: env_type =
  Mstdlib.table @
  (w Numericalp.table) @
  ["Dict", EvtDict (w Dictp.table)] @
  ["String", EvtDict (w Stringp.table)] @
  (w Listp.table) @
  (w Typep.table) @
  ["IO", EvtDict (w Iop.table)]

let purity_table: purityenv_type =
  Mstdlib.purity_table @
  (List.map (fun (k, v) -> (k, get_primitive_purity v)) ocaml_table)