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

let symbol = ['a'-'Z' 'A'-'Z']+
let int = ['0'-'9'] ['0'-'9']*
let white = [' ' '\t' '\r']

rule token = parse
  | white     {token lexbuf}
  | int       { INTEGER (int_of_string (Lexing.lexeme lexbuf))}
  | "true"    { TRUE }
  | "false"   { FALSE }
  | "fun"     { LAMBDA }
  | "lambda"  { LAMBDA }
  | "if"      { IF }
  | "then"    { THEN }
  | "else"    { ELSE }
  | "let"     { LET }
  | "in"      { IN }
  | "->"      { LARROW }
  | "("       { LPAREN }
  | ")"       { RPAREN }
  | "+"       { PLUS }
  | "-"       { MINUS }
  | "*"       { TIMES }
  | "="       { EQUAL }
  | "not"     { NOT }
  | ";;"      { SEMISEMI }
  | symbol    { SYMBOL (Lexing.lexeme lexbuf) }
  | _         { raise (SyntaxError ("Unexpected " ^ Lexing.lexeme lexbuf))}
