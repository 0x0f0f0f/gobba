module T = ANSITerminal
module D = Util.Dict

(** A value identifier*)
type ide = string
[@@deriving show, eq, ord]

(** A type wrapper for complex numbers where equality, ordering
    and showing are defined *)
type complext = Complex.t [@polyprinter fun fmt (n: Complex.t) -> fprintf fmt
                    "%f:+%f" n.re n.im] [@equal (=)] [@compare compare]
[@@deriving show { with_path = false }, eq, ord]

(** A type representing if a computation is pure or not  *)
type puret = Uncertain | Pure | Impure | Numerical
[@@deriving show { with_path = false }, eq, ord]

let isuncertain x = x = Uncertain
let isnumerical x = x = Numerical
let isstrictlypure x = x = Pure
let isimpure x = x = Impure
let ispure x = not (isimpure x)

(** Contains a primitive's name, number of arguments and pureness *)
type primitiveinfo = (ide * int * puret) [@@deriving show { with_path = false }, eq, ord]

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
  | Dict of (ide * expr) list
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
  | Lambda of ide * expr
  | Apply of expr * expr
  | ApplyPrimitive of primitiveinfo * expr list
  | Compose of expr * expr
  | Sequence of expr list
[@@deriving show { with_path = false }, eq, ord]

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
let lambda_from_paramlist l body = List.fold_right (fun p e -> Lambda (p, e)) l body

(** Creates a nested Apply from a list of expressions*)
let apply_from_exprlist l f = List.fold_left (fun e p -> Apply (e, p)) f l

(** Creates a list of Symbol from a list of string*)
let symbols_from_strings l = List.map (fun x -> Symbol x) l

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
  | EvtDict of (ide * evt) list [@equal (=)]
  | Closure of ide * expr * env_type * puret [@equal (=)]
  (** RecClosure keeps the function name in the constructor for recursion *)
  | RecClosure of ide * ide * expr * env_type * puret [@equal (=)]
  (** Abstraction that permits treating primitives as closures *)
[@@deriving show { with_path = false }, eq, ord]

(* Wrapper type that allows both AST expressions and
   evaluated expression for lazy evaluation *)
and type_wrapper =
  | LazyExpression of expr
  | AlreadyEvaluated of evt
[@@deriving show { with_path = false }]

(* An environment of already evaluated values  *)
and env_type = (ide, type_wrapper) D.t [@@deriving show { with_path = false }, eq, ord]

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
  | TLambda of puret

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
  | TLambda p -> (show_puret p) ^ " fun"

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
                    (List.map (fun (x,y) -> x ^ ":" ^ show_unpacked_evt y) d))
                 ^ "}"
  | Closure (param, body, _, _) -> "(fun " ^ (String.concat " " (param::(findparams body))) ^ " -> ... )"
  | RecClosure (name, param, body, _, _) -> name ^ " = (rec fun " ^ (String.concat " " (param::(findparams body))) ^ " -> ... )"

(** Function that creates a list with the params of a nested lambda in a Closure *)
let findevtparams l = match l with
  | Closure(p, b, _, _) -> p::(findparams b)
  | RecClosure(_, p, b, _, _) -> p::(findparams b)
  | _ -> []

(** A type representing a primitive *)
type primitive = Primitive of (evt list -> evt) * primitiveinfo

(** Get the purity of a primitive *)
let get_primitive_purity x = match x with
  Primitive (_, (_, _, p)) -> p

(** Get the actual function from a primitive type *)
let get_primitive_function x = match x with
  Primitive (f, _) -> f

(** Get the information from a primitive type *)
let get_primitive_info x = match x with
  Primitive (_, i) -> i


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

(** The location of a lexeme in code *)
type location =
  | Location of Lexing.position * Lexing.position (** delimited location *)
  | Nowhere (** no location *)

(** Get the location of a lexeme *)
let location_of_lex lex =
  Location (Lexing.lexeme_start_p lex, Lexing.lexeme_end_p lex)

(** Exceptions *)
type internalerrort =
  | Fatal of string
  | InternalFailure of string
  | WrongPrimitiveArgs
  | IndexOutOfBounds
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

(** Print the location of a lexeme*)
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


