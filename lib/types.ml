type ide = string;;

type expr =
    | Integer of int
    | Boolean of bool
    | Symbol of ide
    (* Numerical Operations *)
    | Sum of expr * expr
    | Sub of expr * expr
    | Mult of expr * expr
    | IfThenElse of expr * expr * expr
    | Let of ide * expr * expr
    | Fun of ide list * expr
    | Apply of expr * expr list
    ;;

(* type env_type = (ide, expr) Hashtbl.t *)

type 'a env_t = (string * 'a) list;;

type evt =
    | Int of int
    | Bool of bool
    | Closure of ide list * expr * (evt env_t)

type env_type = evt env_t

exception UnboundVariable;;
