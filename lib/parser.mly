%{
  open Types
%}

%token <string> SYMBOL
%token <int> INTEGER
%token UNIT
%token TRUE FALSE
%token NOT
%token LAND
%token OR
%token PLUS
%token MINUS
%token TIMES
%token EQUAL
%token GREATER
%token LESS
%token IF THEN ELSE
%token SEMI
%token LSQUARE RSQUARE
%token HEAD TAIL CONS
%token LAMBDA
%token LARROW
%token LPAREN RPAREN
%token AND
%token LET LAZY REC IN
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


%start toplevel
%type <Types.expr> toplevel

%%

toplevel:
  | d = ast_expr SEMISEMI
    { d }
  | d = ast_expr EOF
    { d }

assignment:
  | name = SYMBOL EQUAL value = ast_expr
    { (name, value) }

ast_expr:
  | e = ast_app_expr
    { e }
  | l = delimited(LSQUARE, separated_list(SEMI, ast_expr) ,RSQUARE)
    { List (expand_list l) }
  | HEAD e = ast_expr
    { Head e }
  | TAIL e = ast_expr
    { Tail e }
  | e = ast_expr CONS ls = ast_expr
    { Cons (e, ls) }
  | NOT e1 = ast_expr
    { Not e1}
  | e1 = ast_expr PLUS e2 = ast_expr
    { Sum (e1, e2) }
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
  | TRUE
    { Boolean true }
  | FALSE
    { Boolean false }
  | n = INTEGER
    { Integer n }

%%