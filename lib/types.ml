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
type puret =  Impure | Uncertain | Pure | Numerical
[@@deriving show { with_path = false }, eq, ord]

(** Contains a primitive's name, number of arguments and pureness *)
type primitiveinfo = (ide * int * puret) [@@deriving show { with_path = false }, eq, ord]

(** Represents a binary operation kind *)
type binop =
  | Eq | Gt | Lt | Ge | Le | And | Or
  | Plus | Sub | Div | Mult
  | Cons | Concat | Compose  [@@deriving show { with_path = false }, eq, ord]

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
  | Dict of (ide * expr) list
  (* Binary Operation *)
  | Binop of binop * expr * expr
  | Not of expr
  (* Control flow and functions *)
  | IfThenElse of expr * expr * expr
  | Let of assignment_type list * expr
  | Lambda of ide * expr
  | Apply of expr * expr
  | ApplyPrimitive of primitiveinfo * expr list
  | Sequence of expr list
[@@deriving show { with_path = false }, eq, ord]

(* Defines an assignment: laziness, name and value *)
and assignment_type = (bool * ide * expr) [@@deriving show { with_path = false }, eq, ord]

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

(** Show a short representation of an expression (useful for stack traces) *)
let rec simple_show_expr e = match e with
  | NumInt i -> string_of_int i
  | NumFloat i -> string_of_float i
  | NumComplex i -> show_complext i
  | Boolean i -> string_of_bool i
  | String i -> "\"" ^ i ^ "\""
  | Symbol s -> s
  | Apply(Symbol f, b) -> f ^ " (" ^ simple_show_expr b ^ ")"
  | Lambda(p, b) -> "(fun " ^ (String.concat " " (p::(findparams b))) ^ " -> ... )"
  | Let(l, _) -> "let " ^ (String.concat " and" (List.map (fun x -> Util.snd3 x ^ " = ... ") l))
  | Binop(kind, a, b) -> simple_show_expr a ^ " " ^ (show_binop kind) ^ " " ^ simple_show_expr b
  | _ -> "<code>"


(** Creates a nested Lambda from a list of params*)
let lambda_from_paramlist l body = List.fold_right (fun p e -> Lambda (p, e)) l body

(** Creates a nested Apply from a list of expressions*)
let apply_from_exprlist l f = List.fold_left (fun e p -> Apply (e, p)) f l

(** Creates a list of Symbol from a list of string*)
let symbols_from_strings l = List.map (fun x -> Symbol x) l

(** A type containing directives information *)
type directive =
  | Dumpenv
  | Dumppurityenv
  | Includefile of string
  | Includefileasmodule of string * ide option 
  | Setpurity of puret
  | Setverbose of int
[@@deriving show,eq,ord]


(** A type useful for evaluating files, stating if a command is
    an expression or simply a "global" declaration (appended to environment) *)
type command =
  | Directive of directive
  | Expr of expr
  | Def of assignment_type list
[@@deriving show { with_path = false }, eq, ord]


(** A type that represents an evaluated (result of a computation) value *)
type evt =
  | EvtUnit
  | EvtInt of int         [@compare compare]
  | EvtFloat of float     [@compare compare]
  | EvtComplex of complext [@compare compare]
  | EvtBool of bool       [@equal (=)] [@compare compare]
  | EvtString of string   [@equal (=)] [@compare compare]
  | EvtList of evt list   [@equal (=)]
  | EvtDict of (ide * evt) list [@equal (=)]
  (** Recursion is achieved by keeping an optional function name in the constructor *)
  | Closure of ide option * ide * expr * env_type  [@equal (=)]
  | LazyExpression of expr
[@@deriving show { with_path = false }, eq, ord]

(* An environment of already evaluated values  *)
and env_type = (ide, evt) D.t [@@deriving show { with_path = false }, eq, ord]

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
                    (List.map (fun (x,y) -> x ^ ":" ^ show_unpacked_evt y) d))
                 ^ "}"
  | Closure (name, param, body, _) ->
    (match name with | Some x -> x | None -> "") ^ "(fun " ^ (String.concat " " (param::(findparams body))) ^ " -> ... )"
  | LazyExpression e -> "<lazy>: " ^ (simple_show_expr e)

(** Function that creates a list with the params of a nested lambda in a Closure *)
let findevtparams l = match l with
  | Closure(_, p, b, _) -> p::(findparams b)
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

(** Generate a lambda from a primitive *)
let lambda_from_primitive prim =
    let name, numparams, purity = get_primitive_info prim in
    (* Generate a closure abstraction from a primitive *)
    let primargs = generate_prim_params numparams in
    let symprimargs = symbols_from_strings primargs in
    let lambdas = lambda_from_paramlist primargs (ApplyPrimitive((name, numparams, purity), symprimargs)) in
    lambdas

(** An environment type containing identifier - purity couples *)
type purityenv_type = (ide, puret) Util.Dict.t [@@deriving show]

(** A recursive type representing a stacktrace frame *)
type stackframe =
  | StackValue of int * expr * stackframe
  | EmptyStack
[@@deriving show { with_path = false }]

(** Push an AST expression into a stack
    @param s The stack where to push the expression
    @param e The expression to push
*)
let push_stack (s: stackframe) (e: expr) =
  match s with
  | StackValue(d, ee, ss) -> StackValue(d+1, e, StackValue(d, ee, ss))
  | EmptyStack -> StackValue(1, e, EmptyStack)

(** Pop an AST expression from a stack *)
let pop_stack (s: stackframe) = match s with
  | StackValue(_, _, ss) -> ss
  | EmptyStack -> failwith "Stack underflow"

let depth_of_stack (s: stackframe) = match s with
  | StackValue(d, _, _) -> d
  | EmptyStack -> 0

let rec string_of_stack maxdepth (s: stackframe) =
  match s with
  | EmptyStack -> "----- : toplevel"
  | StackValue(d, e, ss) ->
    if maxdepth = 0 then "... " ^ (string_of_int d) ^ " stack frames omitted ..." else
    Printf.sprintf "%05i : %s in\n%s" d (simple_show_expr e) (string_of_stack (maxdepth - 1)  ss)

(** Options for the eval function *)
type evalstate = {
  env: env_type;
  purityenv: purityenv_type;
  verbosity: int;
  stack: stackframe;
  mutable printresult: bool;
  purity: puret;
}

