open Lexing
open Printf

module T = ANSITerminal

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
let print_message ?color:(color=T.Default) ?(loc=Nowhere) msg_type =
  match loc with
  | Location _ ->
    T.eprintf [T.Foreground color] "%s" (Format.asprintf "%s at %t:@\n" msg_type (print_location loc));
    Format.kfprintf (fun ppf -> Format.fprintf ppf "@.") Format.err_formatter
  | Nowhere ->
    T.eprintf [T.Foreground color] "%s: " msg_type ;
    Format.kfprintf (fun ppf -> Format.fprintf ppf "@.") Format.err_formatter

(** Print the caught error *)
let print_error (loc, err_type, msg) = print_message ~color:T.Red ~loc err_type "%s" msg

(** A fatal error reported by the toplevel. *)
let fatal_error msg = error ~kind:"Fatal error" msg

(** A syntax error reported by the toplevel *)
let syntax_error ?loc msg = error ~kind:"Syntax error" ?loc msg


(** Parser wrapper that catches syntax-related errors and converts them to errors. *)
let wrap_syntax_errors parser lex =
  try parser lex
  with
  | Types.SyntaxError a  ->
    syntax_error ~loc:(location_of_lex lex) (Scanf.format_from_string a "")
  | e ->
    syntax_error ~loc:(location_of_lex lex) (Scanf.format_from_string (Printexc.to_string e) "")

let print_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  sprintf "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)
