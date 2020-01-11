(** A value identifier*)
type ide = string
[@@deriving show, eq, ord]

(** A type wrapper for complex numbers where equality, ordering
and showing are defined *)
type complext = Complex.t [@polyprinter fun fmt (n: Complex.t) -> fprintf fmt
"%f+%fi" n.re n.im] [@equal (=)] [@compare compare]
[@@deriving show { with_path = false }, eq, ord]

(** A type representing if a computation is pure or not  *)
type puret = Uncertain | Pure | Impure [@@deriving show { with_path = false },
eq, ord]

(** The type representing Abstract Syntax Tree expressions *)
type expr =
  | Unit
  | Purity of puret * expr
  | NumInt of int
  | NumFloat of float
  | NumComplex of complext
  | Boolean of bool
  | String of string
  | Symbol of ide
  | List of expr list
  | Cons of expr * expr
  | ConcatLists of expr * expr
  | ConcatStrings of expr * expr
  | Dict of (expr * expr) list
  (* Numerical Operations *)
  | Plus of (expr * expr)
  | Sub of (expr * expr)
  | Div of (expr * expr)
  | Mult of (expr * expr)
  (* Boolean Operations *)
  | Eq of expr * expr
  | Gt of expr * expr
  | Lt of expr * expr
  | Ge of expr * expr
  | Le of expr * expr
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

(** A type useful for evaluating files, stating if a command is
    an expression or simply a "global" declaration (appended to environment) *)
type command =
  | Expr of expr
  | Def of (ide * expr) list
  | Defrec of (ide * expr) list
[@@deriving show { with_path = false }, eq, ord]

(** A purely functional environment type, parametrized *)
type 'a env_t = (string * 'a) list [@@deriving show { with_path = false }, eq, ord]

(** A type that represents an evaluated (reduced) value *)
type evt =
  | EvtUnit
  | EvtInt of int         [@compare compare]
  | EvtFloat of float     [@compare compare]
  | EvtComplex of complext [@compare compare]
  | EvtBool of bool       [@equal (=)] [@compare compare]
  | EvtString of string   [@equal (=)] [@compare compare]
  | EvtList of evt list   [@equal (=)]
  | EvtDict of (evt * evt) list [@equal (=)]
  | Closure of ide list * expr * (type_wrapper env_t) [@equal (=)]
  (** RecClosure keeps the function name in the constructor for recursion *)
  | RecClosure of ide * ide list * expr * (type_wrapper env_t) [@equal (=)]
  (** Abstraction that permits treating primitives as closures *)
  | PrimitiveAbstraction of primitivet
[@@deriving show { with_path = false }, eq, ord]
(* Wrapper type that allows both AST expressions and
   evaluated expression for lazy evaluation *)
and type_wrapper =
  | LazyExpression of expr
  | AlreadyEvaluated of evt
[@@deriving show { with_path = false }]
(* Primitive abstraction type *)
and primitivet =
  (ide * int * (type_wrapper env_t) * puret)
[@@deriving show { with_path = false }]


(* Generate a list of parameter names to use in the primitive abstraction *)
let generate_prim_params n =
  if n = 0 then ["..."] else
  Array.to_list(Array.make n 'a' |> Array.mapi (fun i c -> int_of_char c + i |> char_of_int |> Printf.sprintf "%c"))


let rec show_unpacked_evt e = match e with
  | EvtUnit -> "()"
  | EvtInt v -> string_of_int v
  | EvtFloat v -> Printf.sprintf "%f" v
  | EvtComplex n -> show_complext n
  | EvtBool v -> string_of_bool v
  | EvtString v -> "\"" ^ (String.escaped v) ^ "\""
  | EvtList l -> "[" ^ (String.concat "; " (List.map show_unpacked_evt l)) ^ "]"
  | EvtDict d -> "{" ^ 
                 (String.concat ", " 
                    (List.map (fun (x,y) -> show_unpacked_evt x ^ ":" ^ show_unpacked_evt y) d)) 
                 ^ "}"
  | Closure (params, _, _) -> "(fun " ^ (String.concat " " params) ^ " -> ... )"
  | RecClosure (name, params, _, _) -> name ^ " = (rec fun " ^ (String.concat " " params) ^ " -> ... )"
  | PrimitiveAbstraction (name, numargs, _ , _) -> name ^ " = " ^ "(fun " ^ (generate_prim_params numargs |> String.concat " ") ^ " -> ... )"

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

let depth_of_stack (s: stackframe) = match s with
  | StackValue(d, _, _) -> d
  | EmptyStack -> 0

(** Options for the eval function, includes *)
type evalstate = {
  env: env_type;
  verbosity: int;
  stack: stackframe;
  printresult: bool;
  pureness: puret;
}


(** Exceptions *)
exception UnboundVariable of string
exception WrongPrimitiveArgs
exception TypeError of string
exception ListError of string
exception DictError of string
exception SyntaxError of string
exception FileNotFoundError of string
exception PurityError of string