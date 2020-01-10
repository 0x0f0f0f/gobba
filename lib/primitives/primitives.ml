open Util
open Types

(** Wrap non-special primitives (those that do note use apply and eval) into a
    general type constructor *)
let wrapprim table =
  (zip (fstl table) (List.map (fun (f, numargs) -> ((fun args _ _ -> f args), numargs)) (sndl table)))

let table: (string * ((evt list -> (evt -> type_wrapper list -> evalopts -> evt) -> evalopts -> evt) * int)) list
  = (wrapprim Numerical.table) @ (wrapprim Dictp.table)
  @ (wrapprim Listp.table) @ (wrapprim Stringp.table) @ Special.table

let unsafe_table: (string * ((evt list -> (evt -> type_wrapper list -> evalopts -> evt) -> evalopts -> evt) * int)) list
  = (wrapprim Iop.table)

let fulltable = unsafe_table @ table

let jsprelude =  Dictp.js ^ Listp.js ^ Special.js

let getfun f = (fst (Dict.get f table))