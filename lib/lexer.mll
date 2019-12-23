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
  | white       {token lexbuf}
  | '\n'        { Lexing.new_line lexbuf; token lexbuf }
  | int         { INTEGER (int_of_string (Lexing.lexeme lexbuf))}
  | "()"        { UNIT }
  | "true"      { TRUE }
  | "false"     { FALSE }
  | '"'         { read_string (Buffer.create 17) lexbuf }
  | "fun"       { LAMBDA }
  | "lambda"    { LAMBDA }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "let"       { LET }
  | "and"       { AND }
  | "lazy"      { LAZY }
  | "rec"       { REC }
  | "->"        { LARROW }
  | "in"        { IN }
  | "["         { LSQUARE }
  | "]"         { RSQUARE }
  | "("         { LPAREN }
  | ")"         { RPAREN }
  | "head"      { HEAD }
  | "tail"      { TAIL }
  | "::"        { CONS }
  | "&&"        { LAND }
  | "||"        { OR }
  | "+"         { PLUS }
  | "-"         { MINUS }
  | "*"         { TIMES }
  | "="         { EQUAL }
  | ">"         { GREATER }
  | "<"         { LESS }
  | "not"       { NOT }
  | ">=>"       { PIPE }
  | ";"         { SEMI }
  | ";;"        { SEMISEMI }
  | symbol      { SYMBOL (Lexing.lexeme lexbuf) }
  | eof         { EOF }
  | _           { raise (SyntaxError ("Unexpected symbol" ^ Lexing.lexeme lexbuf))}

and read_string buf = parse 
  | '"'         { STRING (Buffer.contents buf) }
  | '\\' '/'    { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\'    { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'a'    { Buffer.add_char buf '\007'; read_string buf lexbuf }
  | '\\' 'b'    { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'    { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'    { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'    { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'    { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | '\\' '"'    { Buffer.add_char buf '"'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }
  | _
    { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof         { raise (SyntaxError ("Unterminated string")) }