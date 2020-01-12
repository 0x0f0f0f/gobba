open Util
open Types

let table = Numerical.table @ Dictp.table
  @ Listp.table @ Stringp.table
  @ Typep.table @ Iop.table

let stdlib_table: ((ide, expr) Dict.t) = []
