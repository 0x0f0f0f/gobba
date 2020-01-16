open Types

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
exception InternalError of (location * internalerrort * stackframe)

(** Utility function to raise a syntax error quickly *)
let sraises l msg s = raise (InternalError ((location_of_lex l), SyntaxError msg, s))
let sraise l msg = raise (InternalError ((location_of_lex l), SyntaxError msg, EmptyStack))


(** Utility function to raise an internal error without a location*)
let iraises e s = raise (InternalError (Nowhere, e, s))
let iraise e = raise (InternalError (Nowhere, e, EmptyStack))

(** Utility function to raise a type error without a location*)
let traises msg s = raise (InternalError (Nowhere, TypeError msg, s))
let traise msg = raise (InternalError (Nowhere, TypeError msg, EmptyStack))


(** Print the location of a lexeme*)
let print_location loc  =
  match loc with
  | Nowhere ->
    "unknown location"
  | Location (begin_pos, end_pos) ->
    let begin_char = begin_pos.Lexing.pos_cnum - begin_pos.Lexing.pos_bol in
    let end_char = end_pos.Lexing.pos_cnum - begin_pos.Lexing.pos_bol in
    let begin_line = begin_pos.Lexing.pos_lnum in
    let filename = begin_pos.Lexing.pos_fname in

    if String.length filename != 0 then
      Printf.sprintf "file %S, line %d, charaters %d-%d" filename begin_line begin_char end_char
    else
      Printf.sprintf "line %d, characters %d-%d" (begin_line - 1) begin_char end_char

(** Print a message at a given location [loc] of message type [msg_type]. *)
let print_message ?color:(color=T.Default) ?(loc=Nowhere) header contents =
  flush_all ();
  match loc with
  | Location _ ->
    T.eprintf [T.Foreground color] "%s: " header; flush_all ();
    Printf.eprintf "at %s\n%s\n%!" (print_location loc) contents;
  | Nowhere ->
    T.eprintf [T.Foreground color] "%s: " header; flush_all ();
    Printf.eprintf "%s\n%!" contents

(** Print the caught error *)
let print_error (loc, err, _) = print_message ~color:T.Red ~loc "Error" (show_internalerrort err)

let print_stacktrace (_, _, s) maxdepth = print_message ~color:T.Red ~loc:Nowhere
  "Stacktrace" ("\n" ^ (string_of_stack maxdepth s))

