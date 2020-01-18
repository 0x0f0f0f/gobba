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


(** A type containing all the types that a values can assume *)
type typeinfo =
  | TVect of int * typeinfo
  | TUnit
  | TBool
  | TNumber
  | TInt
  | TFloat
  | TComplex
  | TString
  | TChar
  | TList
  | TDict
  | TLambda
  [@@deriving eq, ord]

let rec show_typeinfo t = match t with
  | TVect (length, x) ->  "vect of " ^ (string_of_int length) ^ " " ^ (show_typeinfo x)
  | TUnit   -> "unit"
  | TBool   -> "bool"
  | TNumber -> "number"
  | TInt    -> "int"
  | TFloat  -> "float"
  | TComplex -> "complex"
  | TString -> "string"
  | TChar -> "char"
  | TList -> "list"
  | TDict -> "dict"
  | TLambda -> "fun"

(** An environment type containing identifier - purity couples *)
type purityenv_type = (ide, puret) Util.Dict.t [@@deriving show,eq, ord]

(** A type representing if a computation is pure or not  *)
and puret =  Impure | Uncertain | PurityModule of purityenv_type | Pure | Numerical
[@@deriving show { with_path = false }, eq, ord]


(** Contains a primitive's name, number of arguments and pureness *)
type primitiveinfo = (ide * int * puret) [@@deriving show { with_path = false }, eq, ord]

(** Represents a binary operation kind *)
type binop =
  | Getkey
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
  | Character of char
  | Boolean of bool
  | String of string
  | Symbol of ide
  | List of expr list
  | Vect of expr list
  | Dict of assignment_type list
  (* Binary Operation *)
  | Binop of binop * expr * expr
  | Not of expr
  (* Control flow and functions *)
  | IfThenElse of expr * expr * expr
  | Let of assignment_type list * expr
  | Lambda of ide * expr
  | Apply of expr * expr
  | ApplyPrimitive of primitiveinfo * expr list
  | Sequence of expr * expr
[@@deriving show { with_path = false }, eq, ord]

(* Defines an assignment: laziness, name and value *)
and assignment_type = (bool * ide * expr) [@@deriving show { with_path = false }, eq, ord]

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
  | EvtChar of char
  | EvtString of string   [@equal (=)] [@compare compare]
  | EvtList of evt list   [@equal (=)]
  | EvtVect of (typeinfo * evt vect_type) [@printer fun fmt (tinfo, _) -> fprintf fmt "(%s, <vector>)" (show_typeinfo tinfo)]
  | EvtDict of (ide * evt) list [@equal (=)]
  (** Recursion is achieved by keeping an optional function name in the constructor *)
  | Closure of ide option * ide * expr * env_type  [@equal (=)]
  | LazyExpression of expr
[@@deriving show { with_path = false }, eq, ord]

(* An environment of already evaluated values  *)
and env_type = (ide, evt) D.t [@@deriving show { with_path = false }, eq, ord]

(* A type wrapper for vectors where equality, ordering
    and showing are defined  *)
and 'a vect_type = 'a array [@@deriving show, eq,ord]


(** A type representing a primitive *)
type primitive = Primitive of (evt list -> evt) * primitiveinfo


(** A recursive type representing a stacktrace frame *)
type stackframe =
  | StackValue of int * expr * stackframe
  | EmptyStack
[@@deriving show { with_path = false }]

(** Options for the eval function *)
type evalstate = {
  env: env_type;
  purityenv: purityenv_type;
  verbosity: int;
  stack: stackframe;
  mutable printresult: bool;
  purity: puret;
}

