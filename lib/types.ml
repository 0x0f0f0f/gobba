module T = ANSITerminal

(** A value identifier*)
type ide = string
[@@deriving show, eq, ord]

(** A type wrapper for complex numbers where equality, ordering
    and showing are defined *)
type complext = Complex.t [@polyprinter fun fmt (n: Complex.t) -> fprintf fmt
                    "%f:+%f" n.re n.im] [@equal (=)] [@compare compare]
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
  | Closure of ide list * expr * env_type [@equal (=)]
  (** RecClosure keeps the function name in the constructor for recursion *)
  | RecClosure of ide * ide list * expr * env_type [@equal (=)]
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
  (ide * int * env_type * puret)
[@@deriving show { with_path = false }]

(* An environment of already evaluated values  *)
and env_type = (ide, type_wrapper) Util.Dict.t [@@deriving show { with_path = false }, eq, ord]

(** A type containing information about types *)
and typeinfo =
  | TUnit
  | TBool
  | TNumber
  | TInt
  | TFloat
  | TComplex
  | TString
  | TList
  | TDict
  | TLambda

let show_tinfo t = match t with
  | TUnit   -> "unit"
  | TBool   -> "bool"
  | TNumber -> "number"
  | TInt    -> "int"
  | TFloat  -> "float"
  | TComplex -> "complex"
  | TString -> "string"
  | TList -> "list"
  | TDict -> "dict"
  | TLambda -> "fun"

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
  purity: puret;
}

type location =
  | Location of Lexing.position * Lexing.position (** delimited location *)
  | Nowhere (** no location *)

let location_of_lex lex =
  Location (Lexing.lexeme_start_p lex, Lexing.lexeme_end_p lex)

(** Exceptions *)
type internalerrort =
  | Fatal of string
  | WrongPrimitiveArgs
  | TypeError of string
  | UnboundVariable of string
  | ListError of string
  | DictError of string
  | FileNotFoundError of string
  | PurityError of string
  | SyntaxError of string
[@@deriving show { with_path = false }]

(** Exception [Error (loc, err, msg)] indicates an error of type [err] with error message
    [msg], occurring at location [loc]. *)
exception InternalError of (location * internalerrort)

(** Utility function to raise a syntax error quickly *)
let sraise l msg = raise (InternalError ((location_of_lex l), SyntaxError msg))

(** Utility function to raise an internal error without a location*)
let iraise e = raise (InternalError (Nowhere, e))

(** Utility function to raise a type error without a location*)
let traise msg = raise (InternalError (Nowhere, TypeError msg))


let print_location loc ppf =
  match loc with
  | Nowhere ->
    Format.fprintf ppf "unknown location"
  | Location (begin_pos, end_pos) ->
    let begin_char = begin_pos.Lexing.pos_cnum - begin_pos.Lexing.pos_bol in
    let end_char = end_pos.Lexing.pos_cnum - begin_pos.Lexing.pos_bol in
    let begin_line = begin_pos.Lexing.pos_lnum in
    let filename = begin_pos.Lexing.pos_fname in

    if String.length filename != 0 then
      Format.fprintf ppf "file %S, line %d, charaters %d-%d" filename begin_line begin_char end_char
    else
      Format.fprintf ppf "line %d, characters %d-%d" (begin_line - 1) begin_char end_char

(** Print a message at a given location [loc] of message type [msg_type]. *)
let print_message ?color:(color=T.Default) ?(loc=Nowhere) msg_type =
  match loc with
  | Location _ ->
    T.eprintf [T.Foreground color] "%s" (Format.asprintf "%s at %t:@\n" msg_type (print_location loc));
    Format.kfprintf (fun ppf -> Format.fprintf ppf "@.") Format.err_formatter
  | Nowhere ->
    T.eprintf [T.Foreground color] "%s: " msg_type ;
    Format.kfprintf (fun ppf -> Format.fprintf ppf "@.") Format.err_formatter

(** Print the caught error *)
let print_error (loc, err) = print_message ~color:T.Red ~loc "Error" "%s" (show_internalerrort err)


