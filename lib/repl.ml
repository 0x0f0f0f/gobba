open Types
open Eval
open Lexing
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

let read_toplevel parser () =
    let prompt = "> "
    and prompt_more = "> " in
    print_string prompt ;
    let str = ref (read_line ()) in
      while String.length !str > 0 && !str.[String.length !str - 1] == '\\' do
        print_string prompt_more ;
        str := String.sub !str 0 (String.length !str - 1) ^ "\n" ^ (read_line ())
      done ;
      parser (Lexing.from_string (!str ^ "\n"))

 (** Parser wrapper that catches syntax-related errors and converts them to errors. *)
  let wrap_syntax_errors parser lex =
    try parser lex
    with
      | Failure _ ->
        syntax_error ~loc:(location_of_lex lex) "unrecognised symbol"
      | _ ->
        syntax_error ~loc:(location_of_lex lex) "syntax error"

let print_position lexbuf =
    let pos = lexbuf.lex_curr_p in
    sprintf "%s:%d:%d" pos.pos_fname pos.pos_lnum
        (pos.pos_cnum - pos.pos_bol + 1)

let parser = Parser.toplevel Lexer.token

let rec read_lines_until ic del =
    let line = input_line ic in
        if (String.length line) < (String.length del) then
            line
        else if (String.sub (String.trim line)
            ((String.length line) - (String.length del))
            (String.length del)) = del
        then line
        else line ^ (read_lines_until ic del)

let repl env =
    try
    while true do
        try
        let command = read_toplevel (wrap_syntax_errors parser) () in
        let evaluated = eval command env in
        print_endline (show_expr command);
        print_endline (show_evt evaluated);
        with
            | Error err -> print_error err
            | Sys.Break -> prerr_endline "Interrupted."
    done
    with
      | End_of_file -> prerr_endline "Goodbye!"
