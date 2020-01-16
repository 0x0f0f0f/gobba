%{
  open Types
%}

%token <string> SYMBOL
%token <int> INTEGER
%token <float> FLOAT
%token CPLUS CMIN
%token <string> STRING
%token UNIT
%token <bool> BOOLEAN
%token DOT
%token NOT
%token LAND
%token OR
%token ATSIGN
%token CONCAT
%token PLUS
%token MINUS
%token TIMES
%token TOPOWER
%token DIV
%token EQUAL DIFFER GREATER GREATEREQUAL LESS LESSEQUAL
%token IF THEN ELSE
%token SEMI
%token LSQUARE RSQUARE
%token CONS
%token COLON COMMA
%token LBRACKET RBRACKET
%token LAMBDA
%token LARROW
%token LPAREN RPAREN
%token AND
%token LET LAZY IN
%token PIPE COMPOSE
%token PURE IMPURE
%token DOLLAR
%token SEMISEMI
%token <string> DIRECTIVE
%token EOF

/* Associativity of operators */

%left PIPE
%nonassoc LAMBDA LARROW
%nonassoc IF THEN ELSE
%left LAND OR
%left PLUS MINUS
%left TIMES
%left TOPOWER
%left DIV
%left EQUAL DIFFER GREATER GREATEREQUAL LESS LESSEQUAL
%left DOT

%start toplevel
%type <Types.command list> toplevel

%%

optterm_list(separator, X):
  | separator? {[]}
  | l=optterm_nonempty_list(separator, X) { l } 
optterm_nonempty_list(separator, X):
  | x = X separator? { [ x ] }
  | x = X
    separator
    xs = optterm_nonempty_list(separator, X)
     { x :: xs }

toplevel: l = optterm_list(SEMISEMI, statement ); EOF { l }

statement:
  | e = ast_expr
    { Expr(e) }
  | d = def 
  { Def d  }
  | d = directive 
  { Directive d }

assignment:
  | name = SYMBOL EQUAL value = ast_expr
  { (false, name, value) }
  | LAZY name = SYMBOL EQUAL value = ast_expr
  { (true, name, value) }

def:
  | LET a = separated_list(AND, assignment)
    { a }

directive:
  | s = DIRECTIVE a = STRING
  { match s with
    | "#include" -> Includefile a
    | "#module" -> Includefileasmodule (a, None)
    | _ -> failwith "unknown directive" }
  | s = DIRECTIVE i = INTEGER
  { match s with
    | "#verbose" -> Setverbose i
    | _ -> failwith "unknown directive" }
  | s = DIRECTIVE UNIT
  { match s with
    | "#pure"   -> Setpurity Pure
    | "#impure"   -> Setpurity Impure
    | "#uncertain" -> Setpurity Uncertain
    | "#dumppurityenv" -> Dumppurityenv
    | "#dumpenv" -> Dumpenv
    | _ -> failwith "unknown directive" }


ast_expr:
  | e = ast_app_expr
  { e }
  | l = delimited(LPAREN, separated_nonempty_list(SEMI, ast_expr), RPAREN)
  { Sequence l }
  | e = ast_expr; CONS ls = ast_expr
  { Binop(Cons, e, ls) }
  | NOT e1 = ast_expr
  { Not e1}
  | e1 = ast_expr; ATSIGN; e2 = ast_expr
  { Apply(Apply(Symbol "nth", e2), e1) }
  | e1 = ast_expr; CONCAT; e2 = ast_expr
  { Binop(Concat, e1, e2) }
  | e1 = ast_expr; LAND; e2 = ast_expr
  { Binop(And, e1, e2)}
  | e1 = ast_expr; OR; e2 = ast_expr
  { Binop(Or, e1, e2)}
  | e1 = ast_expr; PLUS; e2 = ast_expr
  { Binop(Plus, e1, e2) }
  | e1 = ast_expr; MINUS; e2 = ast_expr
  { Binop(Sub, e1, e2) }
  | e1 = ast_expr; TIMES; e2 = ast_expr
  { Binop(Mult, e1, e2) }
  | e1 = ast_expr; DIV; e2 = ast_expr
  { Binop(Div, e1, e2) }
  | e1 = ast_expr; EQUAL; e2 = ast_expr
  { Binop(Eq, e1, e2) }
  | e1 = ast_expr; DIFFER; e2 = ast_expr
  { Not(Binop(Eq, e1, e2)) }
  | e1 = ast_expr; GREATER; e2 = ast_expr
  { Binop(Gt, e1, e2) }
  | e1 = ast_expr; LESS; e2 = ast_expr
  { Binop(Lt, e1, e2) }
  | e1 = ast_expr; GREATEREQUAL; e2 = ast_expr
  { Binop(Ge, e1, e2) }
  | e1 = ast_expr; LESSEQUAL; e2 = ast_expr
  { Binop(Le, e1, e2) }
  | IF g = ast_expr; THEN b = ast_expr; ELSE e = ast_expr
  { IfThenElse (g, b, e)}
  | d = def IN body = ast_expr
  { Let (d, body) }
  | LAMBDA params = SYMBOL+ LARROW body = ast_expr
  { lambda_from_paramlist params body }
  | e1 = ast_expr; COMPOSE; e2 = ast_expr
  { Binop(Compose, e1, e2) }
  | e1 = ast_expr; PIPE  e2 = ast_expr
  { Binop(Compose, e2, e1) }

ast_app_expr:
  | e = ast_simple_expr
  { e }
  | e1 = ast_app_expr arg = ast_simple_expr
  { Apply (e1, arg)}

ast_simple_expr:
  | var = SYMBOL
  { Symbol var }
  | UNIT
  { Unit }
  | DOLLAR e = ast_expr
  { e }
  | LPAREN e = ast_expr; RPAREN
  { e }
  | e = ast_simple_expr; COLON s = SYMBOL
  { Binop(Getkey, e, Symbol(s)) }
  | PURE e = ast_expr
  { Purity (Pure, e)}
  | IMPURE e = ast_expr
  { Purity (Impure, e)}
  | l = delimited(LSQUARE, separated_list(SEMI, ast_expr), RSQUARE)
  { List l }
  | l = delimited(LBRACKET, separated_list(SEMI, assignment), RBRACKET)
  { Dict l }
  | b = BOOLEAN
  { Boolean b }
  | s = STRING
  { String s }
  | n = INTEGER
  { NumInt n }
  | n = FLOAT
  { NumFloat n }
  | r = FLOAT CPLUS i = FLOAT
  { NumComplex {Complex.re = r; Complex.im = i} }
    | r = FLOAT CMIN i = FLOAT
  { NumComplex {Complex.re = r; Complex.im = -1. *. i} }

%%