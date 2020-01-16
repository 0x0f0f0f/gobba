open Lexing
open Errors

module I = Parser.MenhirInterpreter

let rec parse lexbuf (checkpoint : (Types.command list) I.checkpoint ) =
  match checkpoint with
  | I.InputNeeded _env ->
      let token = Lexer.token lexbuf in
      let startp = lexbuf.lex_start_p
      and endp = lexbuf.lex_curr_p in
      let checkpoint = I.offer checkpoint (token, startp, endp) in
      parse lexbuf checkpoint
  | I.Shifting _
  | I.AboutToReduce _ ->
    let checkpoint = I.resume checkpoint in
    parse lexbuf checkpoint
  | I.HandlingError _env ->
      sraise lexbuf "Syntax error"
  | I.Accepted v -> v
  | I.Rejected ->
      sraise lexbuf "invalid syntax (parser rejected the input)"

(** Parse the contents from a file, using a given [parser]. *)
let read_file fn =
try
  if not (Sys.file_exists fn) then iraise (FileNotFoundError fn) else
    let fh = open_in fn in
    let lex = Lexing.from_channel fh in
    lex.Lexing.lex_curr_p <- {lex.Lexing.lex_curr_p with Lexing.pos_fname = fn};
    try
      let terms = parse lex (Parser.Incremental.toplevel lex.lex_curr_p) in
      close_in fh;
      terms
    with
    (* Close the file in case of any parsing errors. *)
      e -> close_in fh ; iraise (SyntaxError (Printexc.print_backtrace stderr; Printexc.to_string e))
with
(* Any errors when opening or closing a file are fatal. *)
  Sys_error msg -> iraise (Fatal msg)

let read_one str =
  let lex = (Lexing.from_string (str ^ "\n")) in
  parse lex (Parser.Incremental.toplevel lex.lex_curr_p)
