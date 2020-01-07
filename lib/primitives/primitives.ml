open Util
open Types

(** Wrap non-special primitives (those that do note use apply and eval) into a
    general type constructor *)
let wrapprim table =
  (zip (fstl table) (List.map (fun (f, numargs) -> ((fun args _ _ -> f args), numargs)) (sndl table)))

let table: (string * ((evt list -> (evt -> type_wrapper list -> evalopts -> evt) -> evalopts -> evt) * int)) list
  = (wrapprim Dictp.table) @ (wrapprim Listp.table) @ (wrapprim Stringp.table) @ Special.table

let alljs = "{" ^ Ramda.ramda ^ "}\n" ^ Dictp.js ^ Listp.js ^ Special.js