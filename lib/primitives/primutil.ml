open Types
let parser = Parser.toplevel Lexer.token

(** An helper function that helps extracting closures from strings,
to be used as functions in the standard library. An empty environment is used
since primitives in the standard library should not be able to access external values
TODO: compute at compile time *)
let lambda_of_string name str =
   try
      (match (List.hd (parser (Lexing.from_string (str ^ "\n")))) with
      | Expr(Lambda(p, body)) -> LazyExpression (Lambda(p, body))
      | _ -> failwith "standard library definition error")
   with
   | e -> failwith ("standard library definition error in " ^ name ^ ": \n" ^
      (Printexc.print_backtrace stderr; Printexc.to_string e))
