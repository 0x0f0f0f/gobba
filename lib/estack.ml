open Types

(** Push an AST expression into a stack
    @param s The stack where to push the expression
    @param e The expression to push
*)
let push_stack (s: stackframe) (e: expr) =
  match s with
  | StackValue(d, ee, ss) -> StackValue(d+1, e, StackValue(d, ee, ss))
  | EmptyStack -> StackValue(1, e, EmptyStack)

(** Pop an AST expression from a stack *)
let pop_stack (s: stackframe) = match s with
  | StackValue(_, _, ss) -> ss
  | EmptyStack -> failwith "Stack underflow"

let depth_of_stack (s: stackframe) = match s with
  | StackValue(d, _, _) -> d
  | EmptyStack -> 0

let rec string_of_stack maxdepth (s: stackframe) =
  match s with
  | EmptyStack -> "----- : toplevel"
  | StackValue(d, e, ss) ->
    if maxdepth = 0 then "... " ^ (string_of_int d) ^ " stack frames omitted ..." else
    Printf.sprintf "%05i : %s in\n%s" d (Expr.show_expr_short e) (string_of_stack (maxdepth - 1)  ss)
