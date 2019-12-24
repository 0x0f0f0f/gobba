(** A value identifier*)
type ide = string
[@@deriving show, eq, ord]

(** The type representing Abstract Syntax Tree expressions *)
type expr =
    | Unit
    | Integer of int
    | Boolean of bool
    | String of string
    | Symbol of ide
    | List of expr list
    | Cons of expr * expr
    | Dict of (expr * expr) list
    (* Dictionary and list morfisms*)
    | Mapv of expr * expr
    | Fold of expr * expr 
    | Filter of expr * expr
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
    | Sequence of expr list
    | Pipe of expr * expr
    [@@deriving show { with_path = false }, eq, ord]

(** A purely functional environment type, parametrized *)
type 'a env_t = (string * 'a) list [@@deriving show { with_path = false }, eq, ord]

(** A type that represents an evaluated (reduced) value *)
type evt =
    | EvtUnit
    | EvtInt of int         [@compare compare]
    | EvtBool of bool       [@equal (=)] [@compare compare]
    | EvtString of string   [@equal (=)] [@compare compare]
    | EvtList of evt list   [@equal (=)]
    | EvtDict of (evt * evt) list [@equal (=)]
(*     | Primitive of ide list * ide [@equal (=)] *)
    | Closure of ide list * expr * (type_wrapper env_t) [@equal (=)]
    (** RecClosure keeps the function name in the constructor for recursion *)
    | RecClosure of ide * ide list * expr * (type_wrapper env_t) [@equal (=)]
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
    | EvtString v -> "\"" ^ (String.escaped v) ^ "\""
    | EvtList l -> "[" ^ (String.concat "; " (List.map show_unpacked_evt l)) ^ "]"
    | EvtDict d -> "{" ^ 
        (String.concat ", " 
            (List.map (fun (x,y) -> show_unpacked_evt x ^ ":" ^ show_unpacked_evt y) d)) 
            ^ "}"
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
exception TooManyArgs of string
exception WrongBindList
exception WrongPrimitiveArgs
exception TypeError of string
exception ListError of string
exception SyntaxError of string
exception DictError of string