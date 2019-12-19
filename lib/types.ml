(** A value identifier*)
type ide = string
[@@deriving show, eq, ord]

(** The type representing Abstract Syntax Tree expressions *)
type expr =
    | Unit
    | Integer of int
    | Boolean of bool
    | Symbol of ide
    | List of list_pattern
    (* List operations *)
    | Head of expr
    | Tail of expr
    | Cons of expr * expr
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
    | Let of (ide * expr) list * expr
    | Letlazy of (ide * expr) list * expr
    | Letrec of ide * expr * expr
    | Letreclazy of ide * expr * expr
    | Lambda of ide list * expr
    | Apply of expr * expr list
    [@@deriving show { with_path = false }, eq, ord]
and list_pattern = EmptyList | ListValue of expr * list_pattern [@@deriving show { with_path = false } ]
(** A type to build lists, mutually recursive with `expr` *)

(** A purely functional environment type, parametrized *)
type 'a env_t = (string * 'a) list [@@deriving show { with_path = false }, eq, ord]

(** A type that represents an evaluated (reduced) value *)
type evt =
    | EvtUnit
    | EvtInt of int     [@compare fun a b -> compare a b]
    | EvtBool of bool   [@equal fun a b -> a = b]
    | EvtList of evt list  [@equal fun a b -> a = b]
    | Closure of ide list * expr * (type_wrapper env_t) [@equal fun a b -> a = b]
    (** RecClosure keeps the function name in the constructor for recursion *)
    | RecClosure of ide * ide list * expr * (type_wrapper env_t) [@equal fun a b -> a = b]
    [@@deriving show { with_path = false }, eq, ord]
and type_wrapper =
    | LazyExpression of expr
    | AlreadyEvaluated of evt
    [@@deriving show { with_path = false }]
(** Wrapper type that allows both AST expressions and
evaluated expression for lazy evaluation *)

let rec show_unpacked_evt e = match e with
    | EvtInt v -> string_of_int v
    | EvtBool v -> string_of_bool v
    | EvtList l -> "[" ^ (String.concat "; " (List.map show_unpacked_evt l)) ^ "]"
    | Closure (params, _, _) -> "(fun " ^ (String.concat " " params) ^ " -> ... )"
    | RecClosure (name, params, _, _) -> name ^ " = (rec fun " ^ (String.concat " " params) ^ " -> ... )"
    | _ -> show_evt e

(** An environment of already evaluated values  *)
type env_type = type_wrapper env_t 

(** A recursive type representing a stacktrace frame *)
type stackframe =
    | StackValue of int * expr * stackframe
    | EmptyStack
    [@@deriving show { with_path = false }]

(** Convert a native list to an AST list *)
let rec expand_list l = match l with
    | [] -> EmptyList
    | x::xs -> ListValue (x, expand_list xs)

(** Push an AST expression into a stack
    @param s The stack where to push the expression
    @param e The expression to push
*)
let push_stack (s: stackframe) (e: expr) = match s with
    | StackValue(d, ee, ss) -> StackValue(d+1, e, StackValue(d, ee, ss))
    | EmptyStack -> StackValue(1, e, EmptyStack)

(** Pop an AST expression from a stack *)
let pop_stack (s: stackframe) = match s with
    | StackValue(_, _, ss) -> ss
    | EmptyStack -> failwith "Stack underflow"

exception UnboundVariable of string
exception WrongBindList
exception TypeError of string
exception ListError of string
exception SyntaxError of string
