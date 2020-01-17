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

let mapstr =
{| fun f l ->
   let aux = fun f l ->
      (if l = [] then l else (f (head l))::(aux f (tail l))) in
   if typeof l = "list" then aux f l
   else if typeof l = "dict" then
      let keys = Dict:getkeys l and values = Dict:getvalues l in
      Dict:dictfromlists keys (aux f values)
   else failwith "value is not iterable"
|}

let filterstr =
{| fun pred l ->
   if typeof l = "list" then
      let aux = fun f l ->
      if l = [] then l else if f (head l) then
         (head l)::(aux f (tail l))
         else (aux f (tail l)) in
      aux pred l
   else if typeof l = "dict" then
      let aux = fun f kl vl acc ->
         if kl = [] && vl = [] then acc
         else if f (head vl) then
            aux f (tail kl) (tail vl) (Dict:insert (head kl) (head vl) acc)
         else aux f (tail kl) (tail vl) acc in
      aux pred (Dict:getkeys l) (Dict:getvalues l) {}
   else failwith "value is not iterable"
|}

let foldlstr =
{| fun f z l ->
   if typeof l = "list" then
      let aux = fun f z l ->
         if l = [] then z else
         aux f (f z (head l)) (tail l)
      in aux f z l
   else if typeof l = "dict" then
      let aux = fun f z kl vl ->
         if kl = [] && vl = [] then z else
         aux f (f z (head vl)) (tail kl) (tail vl)
      in aux f z (Dict:getkeys l) (Dict:getvalues l)
   else failwith "value is not iterable"
|}

let foldrstr =
{| fun f z l ->
   if typeof l = "list" then
      let aux = fun f z l ->
         if l = [] then z else
         f (head l) (aux f z (tail l))
      in aux f z l
   else if typeof l = "dict" then
      let aux = fun f z kl vl ->
         if kl = [] && vl = [] then z else
         f (head vl) (aux f z (tail kl) (tail vl))
      in aux f z (Dict:getkeys l) (Dict:getvalues l)
   else failwith "value is not iterable"
|}


let map2str =
{| fun f l1 l2 ->
   if typeof l1 = "list" && typeof l2 = "list" then
      let aux = fun f l1 l2 ->
         if length l1 != length l2 then
            failwith "lists are not of equal length"
         else if l1 = [] && l2 = [] then l1 else
         (f (head l1) (head l2)) :: (aux f (tail l1) (tail l2))
      in aux f l1 l2
   else failwith "value is not iterable by map2"
|}

let mapnstr =
{| fun f lists ->
  if (not (lists = [])) then
    if mem [] lists then
     []
    else
      f (map head lists) :: mapn f (map tail lists)
  else []
|}

let table =
  [("map", (lambda_of_string "map" mapstr));
   ("map2", (lambda_of_string "map2" map2str));
   ("mapn", (lambda_of_string "mapn" mapnstr));
   ("filter", (lambda_of_string "filter" filterstr));
   ("foldl", (lambda_of_string "foldl" foldlstr));
   ("foldr", (lambda_of_string "foldr" foldrstr))
  ]

let purity_table = [("map", Pure); ("filter", Pure); ("foldl", Pure); ("foldr", Pure)]