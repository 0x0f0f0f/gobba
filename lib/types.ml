open Printf

(** An identifier*)
type ide = string

type expr =
    | Integer of int
    | Boolean of bool
    | Symbol of ide
    (* Numerical Operations *)
    | Sum of expr * expr
    | Sub of expr * expr
    | Mult of expr * expr
    | Eq of expr * expr
    | Gt of expr * expr
    | Lt of expr * expr
    (* Boolean operations *)
    | And of expr * expr
    | Or of expr * expr
    | Not of expr
    (* Control flow and functions *)
    | IfThenElse of expr * expr * expr
    | Let of ide * expr * expr
    | Letrec of ide * expr * expr
    | Lambda of ide list * expr
    | Apply of expr * expr list

(* Show an AST of type expr as a string *)
let rec show_expr (obj: expr) : string = match obj with
    | Integer i -> sprintf "Integer %d" i
    | Boolean b -> sprintf "Boolean %B" b
    | Symbol s -> sprintf "Symbol %s" s
    (* Numerical Operations *)
    | Sum (a, b) -> sprintf "Sum (%s, %s)" (show_expr a) (show_expr b)
    | Sub (a, b) -> sprintf "Sub (%s, %s)" (show_expr a) (show_expr b)
    | Mult (a, b) -> sprintf "Mult (%s, %s)" (show_expr a) (show_expr b)
    | Eq (a, b) -> sprintf "Eq (%s, %s)" (show_expr a) (show_expr b)
    | Gt (a, b) -> sprintf "Gt (%s, %s)" (show_expr a) (show_expr b)
    | Lt (a, b) -> sprintf "Lt (%s, %s)" (show_expr a) (show_expr b)
    (* Boolean operations *)
    | And (a, b) -> sprintf "And (%s, %s)" (show_expr a) (show_expr b)
    | Or (a, b) -> sprintf "Or (%s, %s)" (show_expr a) (show_expr b)
    | Not a -> sprintf "Not %s" (show_expr a)
    (* Control flow and functions *)
    | IfThenElse (guard, first, alt) ->
        sprintf "IfThenElse (%s, %s, %s)" (show_expr guard) (show_expr first)
        (show_expr alt)
    | Let (name, value, block) -> sprintf "Let (%s, %s, %s)" (name)
        (show_expr value) (show_expr block)
    | Letrec (name, value, block) -> sprintf "Letrec (%s, %s, %s)" (name)
        (show_expr value) (show_expr block)
    | Lambda (params, body) ->
        sprintf "Lambda ([%s], %s)"
            (String.concat "; " params)
            (show_expr body)
    | Apply (func, params) ->
        sprintf "Apply (%s, [%s])" (show_expr func)
            (String.concat "; " (List.map show_expr params))

(* A non purely functional environment *)
(* type env_type = (ide, expr) Hashtbl.t *)

(** A purely functional environment type, parametrized *)
type 'a env_t = (string * 'a) list

(** A type that represents an evaluated (reduced) value *)
type evt =
    | Int of int
    | Bool of bool
    | Closure of ide list * expr * (evt env_t)
    (** RecClosure keeps the function name in the environment for recursion *)
    | RecClosure of ide * ide list * expr * (evt env_t) 

(** Function to get a string representation of an evaluated type *)
let show_evt (obj: evt) : string = match obj with
    | Int i -> string_of_int i
    | Bool b -> string_of_bool b
    | Closure (params, _, _) ->
        String.concat " " (["<fun"] @ params @ ["-> ...>"])
    | RecClosure (name, params, _, _) ->
        String.concat " " (["<" ^ name] @ params @ ["-> ...>"])


(** An environment type with  *)
type env_type = evt env_t

(** Exception to specify an unbound value *)
exception UnboundVariable of string

(** Exception that indicates an erroneous usage of bindlist *)
exception WrongBindList

(** Exception to represent a syntax error*)
exception SyntaxError of string
