open Util
open Types

let table = Numericalp.table @ Dictp.table
  @ Listp.table @ Stringp.table
  @ Typep.table @ Iop.table

let stdlib_table: ((ide, evt) Dict.t) = Mstdlib.table
