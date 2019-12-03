{
  open Parser
  open Types
  open Lexing

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
        pos_lnum = pos.pos_lnum + 1
      }
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let symbol = alpha (alpha|digit)*
let int = '-'? ['0'-'9'] ['0'-'9']*
let white = [' ' '\t' '\r']

rule token = parse
  | white     {token lexbuf}
  | '\n'      { Lexing.new_line lexbuf; token lexbuf }
  | int       { INTEGER (int_of_string (Lexing.lexeme lexbuf))}
  | "true"    { TRUE }
  | "false"   { FALSE }
  | "fun"     { LAMBDA }
  | "if"      { IF }
  | "then"    { THEN }
  | "else"    { ELSE }
  | "let"     { LET }
  | "rec"     { REC }
  | "lambda"  { LAMBDA }
  | "->"      { LARROW }
  | "in"      { IN }
  | "("       { LPAREN }
  | ")"       { RPAREN }
  | "+"       { PLUS }
  | "-"       { MINUS }
  | "*"       { TIMES }
  | "="       { EQUAL }
  | ">"       { GREATER }
  | "<"       { LESS }
  | "not"     { NOT }
  | ";;"      { SEMISEMI }
  | symbol    { SYMBOL (Lexing.lexeme lexbuf) }
  | eof       { EOF }
  | _         { raise (SyntaxError ("Unexpected " ^ Lexing.lexeme lexbuf))}
