%{
  open Types
%}

%token <string> SYMBOL
%token <int> INTEGER
%token <string> STRING
%token UNIT
%token TRUE FALSE
%token NOT
%token LAND
%token OR
%token PLUS
%token MINUS
%token TIMES
%token EQUAL
%token GREATER GREATEREQUAL LESS LESSEQUAL
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
%token LET LAZY REC IN
%token PIPE
%token SEMISEMI
%token EOF

/* Associativity of operators */
%nonassoc EQUAL
%nonassoc LAMBDA
%nonassoc LARROW
%nonassoc ELSE
%nonassoc THEN
%nonassoc IF
%left PLUS MINUS
%left TIMES
%left PIPE

%start file
%type <Types.command list> file

%start toplevel
%type <Types.command> toplevel

%%

file:
  | EOF
    { [] }
  | e = ast_expr EOF
    { [Expr e] }
  | e = ast_expr SEMISEMI lst = file
    { Expr e :: lst }
  | ds = nonempty_list(def) SEMISEMI lst = file
    { ds @ lst }
  | ds = nonempty_list(def) EOF
    { ds }


def:
  | LET a = separated_list(AND, assignment)
    { Def a }
  | LET REC a = separated_list(AND, assignment)
    { Defrec a }

toplevel:
  | l = separated_list(SEMI, ast_expr) EOF
  { Expr(Sequence(l)) }
  | l = separated_list(SEMI, ast_expr) SEMISEMI
  { Expr(Sequence(l)) }
  | d = def SEMISEMI
  { d }
  | d = def EOF
  { d }
  | d = ast_expr SEMISEMI
  { Expr d }
  | d = ast_expr EOF
  { Expr d }

assignment:
  | name = SYMBOL EQUAL value = ast_expr
  { (name, value) }

dict_value:
  | key = ast_expr COLON value = ast_expr
  { (key, value) }

ast_expr:
  | e = ast_app_expr
  { e }
  | l = delimited(LPAREN, separated_nonempty_list(SEMI, ast_expr), RPAREN)
  { Sequence l }
  | e = ast_expr CONS ls = ast_expr
  { Cons (e, ls) }
  | NOT e1 = ast_expr
  { Not e1}
  | e1 = ast_expr PLUS e2 = ast_expr
  { Plus (e1, e2) }
  | e1 = ast_expr MINUS e2 = ast_expr
  { Sub (e1, e2) }
  | e1 = ast_expr TIMES e2 = ast_expr
  { Mult (e1, e2) }
  | e1 = ast_expr EQUAL e2 = ast_expr
  { Eq (e1, e2) }
  | e1 = ast_expr GREATER e2 = ast_expr
  { Gt (e1, e2) }
  | e1 = ast_expr LESS e2 = ast_expr
  { Lt (e1, e2) }
  | e1 = ast_expr GREATEREQUAL e2 = ast_expr
  { Ge (e1, e2) }
  | e1 = ast_expr LESSEQUAL e2 = ast_expr
  { Le (e1, e2) }
  | e1 = ast_expr LAND e2 = ast_expr
  { And (e1, e2)}
  | e1 = ast_expr OR e2 = ast_expr
  { Or (e1, e2)}
  | IF g = ast_expr THEN b = ast_expr ELSE e = ast_expr
  { IfThenElse (g, b, e)}
  | LET a = separated_list(AND, assignment) IN body = ast_expr
  { Let (a, body) }
  | LET REC name = SYMBOL EQUAL value = ast_expr IN body = ast_expr
  { Letrec (name, value, body) }
  | LET LAZY a = separated_list(AND, assignment) IN body = ast_expr
  { Letlazy (a, body) }
  | LET LAZY REC name = SYMBOL EQUAL value = ast_expr IN body = ast_expr
  { Letreclazy (name, value, body) }
  | LET REC LAZY name = SYMBOL EQUAL value = ast_expr IN body = ast_expr
  { Letreclazy (name, value, body) }
  | LAMBDA params = SYMBOL+ LARROW body = ast_expr
  { Lambda (params, body) }
  | e1 = ast_expr PIPE e2 = ast_expr
  { Pipe(e1, e2) }


ast_app_expr:
  | e = ast_simple_expr
  { e }
  | e1 = ast_app_expr args = ast_simple_expr+
  { Apply (e1, args)}

ast_simple_expr:
  | var = SYMBOL
  { Symbol var }
  | UNIT
  { Unit }
  | LPAREN e = ast_expr RPAREN
  { e }
  | l = delimited(LSQUARE, separated_list(SEMI, ast_expr), RSQUARE)
  { List l }
  | l = delimited(LBRACKET, separated_list(COMMA, dict_value), RBRACKET)
  { Dict l }
  | TRUE
  { Boolean true }
  | FALSE
  { Boolean false }
  | s = STRING
  { String s }
  | n = INTEGER
  { Integer n }

%%