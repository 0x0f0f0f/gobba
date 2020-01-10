open Types


(** Numerical Primitives *)

let int_binop x y op =
  "(" ^ x ^ " " ^ op ^ " " ^ y ^ ")"

let bool_binop x y op =
  "(" ^ x ^ " " ^ op ^ " " ^ y ^ ")"

let bool_unop x op =
  "(" ^ op ^ x ^ ")"

let comparison x y op = "(" ^ x ^ " " ^ op ^ " " ^ y ^ ")"

let dummy = "throw \"NOT YET IMPLEMENTED\";"

let rec compile (e : expr) : string =
  match e with
  | Unit -> "null"
  | NumInt n -> string_of_int n
  | NumFloat n -> string_of_float n
  | Boolean b -> string_of_bool b
  | String s -> "\"" ^ s ^ "\""
  | Symbol x -> x
  | List x -> "[" ^ (String.concat "," (List.map (fun x -> compile x ) x)) ^ "]"
  | Cons (x, xs) -> 
    "R.insert(0," ^
    compile x  ^ "," ^
    compile xs  ^
    ")"
  | ConcatLists(e1, e2) ->
    "R.concat(" ^ compile e1  ^ "," ^ compile e2  ^ ")"
  | ConcatStrings(e1, e2) ->
    "R.concat(" ^ compile e1  ^ "," ^ compile e2  ^ ")"
  (* Dictionaries and operations *)
  | Dict l ->
    "{" ^
    (String.concat "," (List.map (fun (k,v) -> (compile k ) ^ ": " ^ (compile v )) l)) ^ "}"
  | Plus (x, y) -> int_binop (compile x ) (compile y ) "+"
  | Sub (x, y) ->  int_binop (compile x ) (compile y ) "-"
  | Mult (x, y) -> int_binop (compile x ) (compile y ) "*"
  | Div (x, y) -> int_binop (compile x ) (compile y ) "/"
  | And (x, y) ->  bool_binop (compile x ) (compile y ) "+"
  | Or (x, y) ->  int_binop (compile x ) (compile y ) "+"
  | Not x -> bool_unop (compile x ) "!"
  | Eq (x, y) -> "R.equals(" ^ compile x ^ "," ^ compile y ^ ")"
  | Gt (x, y) -> comparison (compile x ) (compile y ) ">"
  | Lt (x, y) -> comparison (compile x ) (compile y ) "<"
  | Ge (x, y) -> comparison (compile x ) (compile y ) ">="
  | Le (x, y) -> comparison (compile x ) (compile y ) "<="
  | IfThenElse (guard, first, alt) ->
    "(" ^ (compile guard) ^ ") ? " ^
    "(" ^ compile first ^ ") : (" ^
    compile alt ^ ")"
  | Let (assignments, body) ->
    "{\n" ^ compile_assignments assignments ^ "(" ^ compile body  ^ ")\n}"
  | Letrec (ident, value, body) -> compile (Let([(ident, value)], body))
  | Lambda (params, body) ->
    "R.curry((" ^ String.concat ", " params ^") => " ^
    (compile body ) ^ ")"
  (* Function Application *)
  | Apply (f, expr_args) -> compile f  ^ tuple expr_args
  (* Eval a sequence of expressions but return the last *)
  | Sequence exprl ->
    "{ " ^ String.concat "; " (List.map (fun x -> compile x ) exprl) ^ " }"
  (* Pipe two functions together, creating a new function
     That uses the first functions's result as the second's first argument *)
  | _ -> dummy
and tuple elems  =
  "(" ^ (String.concat "," (List.map (fun x -> compile x ) elems)) ^ ")"
and compile_assignments ass =
  (String.concat ";\n"
     (List.map (fun (ident, value) ->
          "let " ^ ident ^ " = " ^ (compile value ))
         ass)) ^ ";\n"


let rec compile_cmdlist cmdlist = match cmdlist with
  | [] -> ""
  | x::xs -> (match x with
      | Def(assignments) -> "{ " ^ compile_assignments assignments ^ compile_cmdlist xs ^ "}"
      | Defrec(assignments) -> "{ " ^ compile_assignments assignments ^ compile_cmdlist xs ^ "}"
      | Expr(e) -> compile (Optimizer.optimize e)
      | _ -> dummy)
let compile_program p = compile_cmdlist p
