open Types

let parser = Parser.toplevel Lexer.token

(** An helper function that helps extracting closures from strings,
to be used as functions in the standard library. An empty environment is used
since primitives in the standard library should not be able to access external values
TODO: compute at compile time *)
let closurize name str =
   try
      (match (parser (Lexing.from_string (str ^ "\n"))) with
      | Expr(Lambda(p, body)) -> AlreadyEvaluated (Closure (Some name, p, body, []))
      | _ -> failwith "standard library definition error")
   with
   | e -> failwith ("standard library definition error in " ^ name ^ ": \n" ^
      (Printexc.print_backtrace stderr; Printexc.to_string e))

let mapstr =
{| fun f l ->
   let aux = fun f l ->
      (if l = [] then l else (f (head l))::(aux f (tail l))) in
   if typeof l = "list" then aux f l
   else if typeof l = "dict" then
      let keys = getkeys l and values = getvalues l in
      dictfromlists keys (aux f values)
   else failwith "value is not iterable"
|}

let filterstr =
{| fun pred l ->
   if typeof l = "list" then
       let aux = fun f l ->
      (if l = [] then l else if f (head l) then
         (head l)::(aux f (tail l))
         else (aux f (tail l))) in
      aux pred l
   else if typeof l = "dict" then
      let aux = fun f kl vl acc ->
         if kl = [] && vl = [] then acc
         else if f (head vl) then
            aux f (tail kl) (tail vl) (insert (head kl) (head vl) acc)
         else aux f (tail kl) (tail vl) acc in
      aux pred (getkeys l) (getvalues l) {}
   else failwith "value is not iterable"
|}

let table = [](* 
  [("map", (Pure, closurize "map" mapstr));
   ("filter", (Pure, closurize "filter" filterstr));
  ] *)