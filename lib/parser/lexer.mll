{
  open Parser
  open Errors
  open Lexing

  let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
    pos_lnum = pos.pos_lnum + 1
    }
}

let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let int = '-'? digit digit*
let float = int frac? exp?
let alpha = ['a'-'z' 'A'-'Z']
let symbol = alpha (alpha|digit|'_')*
let white = [' ' '\t' '\r' '\n']

let directive = "#pure" | "#impure" | "#uncertain" | "#dumppurityenv" | "#dumpenv" | "#include" | "#verbose" | "#module" | "#open"

rule token = parse
  | white       { token lexbuf }
  | "(*"        { comments 0 lexbuf }
  | int         { INTEGER (int_of_string (Lexing.lexeme lexbuf))}
  | float       { FLOAT (try float_of_string (Lexing.lexeme lexbuf) with _ -> sraise lexbuf "Invalid use of the dot symbol")}
  | directive   { DIRECTIVE (Lexing.lexeme lexbuf) }
  | "()"        { UNIT }
  | "true"      { BOOLEAN true }
  | "false"     { BOOLEAN false }
  | '\''        { read_char ' ' 1 lexbuf }
  | '"'         { read_string (Buffer.create 17) lexbuf }
  | "fun"       { LAMBDA }
  | "lambda"    { LAMBDA }
  | "λ"         { LAMBDA }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "let"       { LET }
  | "and"       { AND }
  | "lazy"      { LAZY }
  | "->"        { LARROW }
  | "in"        { IN }
  | "pure"      { PURE }
  | "impure"    { IMPURE }
  | "["         { LSQUARE }
  | "]"         { RSQUARE }
  | "[|"        { LVECT }
  | "|]"        { RVECT }
  | "("         { LPAREN }
  | ")"         { RPAREN }
  | "{"         { LBRACKET }
  | "}"         { RBRACKET }
  | ":"         { COLON }
  | ","         { COMMA }
  | "::"        { CONS }
  | "&&"        { LAND }
  | "||"        { OR }
  | "^"         { TOPOWER }
  | "@"         { ATSIGN }
  | ":+"        { COMPLEX }
  | "++"        { CONCAT }
  | "+"         { PLUS }
  | "-"         { MINUS }
  | "*"         { TIMES }
  | "/"         { DIV }
  | "%"         { MODULO }
  | "!="        { DIFFER }
  | "="         { EQUAL }
  | ">"         { GREATER }
  | "<"         { LESS }
  | ">="        { GREATEREQUAL }
  | "<="        { LESSEQUAL }
  | "not"       { NOT }
  | ">>"        { BIND }
  | ">=>"       { PIPE }
  | "<=<"       { COMPOSE }
  | "$"         { DOLLAR }
  | ";"         { SEMI }
  | symbol      { SYMBOL (Lexing.lexeme lexbuf) }
  | eof         { EOF }
  | _           { sraise lexbuf ("Unexpected symbol " ^ Lexing.lexeme lexbuf) }
and comments level = parse
  | "*)"        { if level = 0 then token lexbuf else comments (level - 1) lexbuf}
  | "(*"        { comments (level + 1) lexbuf }
  | _           { comments level lexbuf }
  | eof         { sraise lexbuf "Unterminated comment"}
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
  | _           { sraise lexbuf ("Illegal string character: " ^ Lexing.lexeme lexbuf) }
  | eof         { sraise lexbuf "Unterminated string" }
and read_char mc count = parse
  | '\''        { if count != 0 then sraise lexbuf "Malformed character literal"
                  else CHAR mc}
  | _ as c      { read_char c (count - 1) lexbuf }
  | eof         { sraise lexbuf "Unterminated character" }
