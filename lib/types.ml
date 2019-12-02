open Printf


type location =
  | Location of Lexing.position * Lexing.position (** delimited location *)
  | Nowhere (** no location *)

let location_of_lex lex =
  Location (Lexing.lexeme_start_p lex, Lexing.lexeme_end_p lex)

(** Exception [Error (loc, err, msg)] indicates an error of type [err] with error message
    [msg], occurring at location [loc]. *)
exception Error of (location * string * string)

(** [error ~loc ~kind] raises an error of the given [kind]. The [kfprintf] magic
allows one to write [msg] using a format string. *)

let error ?(kind="Error") ?(loc=Nowhere) =
  let k _ =
    let msg = Format.flush_str_formatter () in
      raise (Error (loc, kind, msg))
  in
    Format.kfprintf k Format.str_formatter

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
let print_message ?(loc=Nowhere) msg_type =
  match loc with
  | Location _ ->
     Format.eprintf "%s at %t:@\n" msg_type (print_location loc) ;
     Format.kfprintf (fun ppf -> Format.fprintf ppf "@.") Format.err_formatter
  | Nowhere ->
     Format.eprintf "%s: " msg_type ;
     Format.kfprintf (fun ppf -> Format.fprintf ppf "@.") Format.err_formatter

(** Print the caught error *)
let print_error (loc, err_type, msg) = print_message ~loc err_type "%s" msg

(** A fatal error reported by the toplevel. *)
let fatal_error msg = error ~kind:"Fatal error" msg

(** A syntax error reported by the toplevel *)
let syntax_error ?loc msg = error ~kind:"Syntax error" ?loc msg

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
    | Lambda (params, body) ->
        sprintf "Lambda (%s, [%s])"
            (String.concat "; " params)
            (show_expr body)
    | Apply (func, params) ->
        sprintf "Apply (%s, [%s])" (show_expr func)
            (String.concat ";" (List.map show_expr params))

(* A non purely functional environment *)
(* type env_type = (ide, expr) Hashtbl.t *)

(** A purely functional environment type, parametrized *)
type 'a env_t = (string * 'a) list

(** A type that represents an evaluated (reduced) value *)
type evt =
    | Int of int
    | Bool of bool
    | Closure of ide list * expr * (evt env_t)

(** Function to get a string representation of an evaluated type *)
let show_evt (obj: evt) : string = match obj with
    | Int i -> string_of_int i
    | Bool b -> string_of_bool b
    | Closure (params, _, _) ->
        String.concat " " (["fun"] @ params @ ["-> ..."])

(** An environment type with  *)
type env_type = evt env_t

(** Exception to specify an unbound value *)
exception UnboundVariable of string

(** Exception that indicates an erroneous usage of bindlist *)
exception WrongBindList

(** Exception to represent a syntax error*)
exception SyntaxError of string
