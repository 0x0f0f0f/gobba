(** A value identifier*)
type ide = string
[@@deriving show]

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
    | Let of ide * expr * expr
    | Letlazy of ide * expr * expr
    | Letrec of ide * expr * expr
    | Lambda of ide list * expr
    | LazyLambda of ide list * expr
    | Apply of expr * expr list
    [@@deriving show { with_path = false }]
and list_pattern = EmptyList | ListValue of expr * list_pattern [@@deriving show { with_path = false }]
(** A type to build lists, mutually recursive with `expr` *)

(** A purely functional environment type, parametrized *)
type 'a env_t = (string * 'a) list [@@deriving show { with_path = false }]

(** A type that represents an evaluated (reduced) value *)
type evt =
    | EvtUnit
    | EvtInt of int
    | EvtBool of bool
    | EvtList of evt list
    | Closure of ide list * expr * (type_wrapper env_t)
    | LazyClosure of ide list * expr * (type_wrapper env_t)
    (** RecClosure keeps the function name in the constructor for recursion *)
    | RecClosure of ide * ide list * expr * (type_wrapper env_t)
    | RecLazyClosure of ide * ide list * expr * (type_wrapper env_t)
    [@@deriving show { with_path = false }]
and type_wrapper =
    | LazyExpression of expr
    | AlreadyEvaluated of evt
    [@@deriving show]
(** Wrapper type that allows both AST expressions and
evaluated expression for lazy evaluation *)

let rec show_unpacked_evt e = match e with
    | EvtInt v -> string_of_int v
    | EvtBool v -> string_of_bool v
    | EvtList l -> "[" ^ (String.concat "; " (List.map show_unpacked_evt l)) ^ "]"
    | Closure (params, _, _) -> "(fun " ^ (String.concat " " params) ^ " -> ... )"
    | LazyClosure (params, _, _) -> "(lazyfun " ^ (String.concat " " params) ^ " -> ... )"
    | RecClosure (name, params, _, _) -> name ^ " = (rec fun " ^ (String.concat " " params) ^ " -> ... )"
    | RecLazyClosure (name, params, _, _) -> name ^ " = (rec lazyfun " ^ (String.concat " " params) ^ " -> ... )"
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

(** Push an AST expression into a stack *)
let push_stack (s: stackframe) (e: expr) = match s with
    | StackValue(d, ee, ss) -> (* if d = 25 then failwith "Stack overflow" else *) StackValue(d+1, e, StackValue(d, ee, ss))
    | EmptyStack -> StackValue(1, e, EmptyStack)

(** Pop an AST expression from a stack *)
let pop_stack (s: stackframe) = match s with
    | StackValue(_, _, ss) -> ss
    | EmptyStack -> failwith "STACK UNDERFLOW"

exception UnboundVariable of string
exception WrongBindList
exception TypeError of string
exception ListError of string
exception SyntaxError of string
