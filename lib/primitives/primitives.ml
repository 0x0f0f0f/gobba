open Util
open Types

(** Wrap non-special primitives (those that do note use apply and eval) into a
    general type constructor *)
let wrapprim table =
  (zip (fstl table) (List.map (fun (f, numargs) -> ((fun args _ _ -> f args), numargs)) (sndl table)))

let table: (string * ((evt list -> (evt -> type_wrapper list -> evalstate -> evt) -> evalstate -> evt) * int)) list
  = (wrapprim Numerical.table) @ (wrapprim Dictp.table)
  @ (wrapprim Listp.table) @ (wrapprim Stringp.table) @ Special.table
  @ (wrapprim Typep.table)
  
let impure_table: (string * ((evt list -> (evt -> type_wrapper list -> evalstate -> evt) -> evalstate -> evt) * int)) list
  = (wrapprim Iop.table)

let fulltable = impure_table @ table

let getfun f = (fst (Dict.get f table))