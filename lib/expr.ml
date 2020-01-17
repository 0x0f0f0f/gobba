(** Contains functions to manipulate expressions *)
open Types

(** Function that finds a nested lambda body *)
let rec findbody l = match l with
  | Lambda(_, b) ->  findbody b
  | other -> other

(** Function that finds and replaces a (nested) lambda body *)
let rec replacebody l newbody = match l with
  | Lambda(p, b) -> Lambda(p, replacebody b newbody)
  | _ -> newbody

(** Function that creates a list with the params of a nested lambda*)
let rec findparams l = match l with
  | Lambda(p, b) -> p::(findparams b)
  | _ -> []

(** Creates a nested Lambda from a list of params*)
let lambda_of_paramlist l body = List.fold_right (fun p e -> Lambda (p, e)) l body

(** Creates a nested Apply from a list of expressions*)
let apply_from_exprlist l f = List.fold_left (fun e p -> Apply (e, p)) f l

(** Creates a list of Symbol from a list of string*)
let symbols_from_strings l = List.map (fun x -> Symbol x) l


(** Show a short representation of an expression (useful for stack traces) *)
let rec show_expr_short e = match e with
  | NumInt i -> string_of_int i
  | NumFloat i -> string_of_float i
  | NumComplex i -> show_complext i
  | Boolean i -> string_of_bool i
  | String i -> "\"" ^ i ^ "\""
  | Symbol s -> s
  | List l -> "[" ^ (String.concat ", " (List.map show_expr_short l)) ^ "]"
  | Dict d -> "{ " ^ (List.fold_left (fun x y -> (x ^ " = ... , " ^ y)) "" (Util.snd3l d)) ^ " }"
  | Apply(Symbol f, b) -> f ^ " (" ^ show_expr_short b ^ ")"
  | Lambda(p, b) -> "(fun " ^ (String.concat " " (p::(findparams b))) ^ " -> ... )"
  | Let(l, _) -> "let " ^ (String.concat " and" (List.map (fun x -> Util.snd3 x ^ " = ... ") l))
  | Binop(kind, a, b) -> show_expr_short a ^ " " ^ (show_binop kind) ^ " " ^ show_expr_short b
  | _ -> "<code>"
