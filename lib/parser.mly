%{
  open Types
%}

%token <int> INTEGER
%token <string> SYMBOL
%token TRUE FALSE
%token NOT
%token AND
%token OR
%token PLUS
%token MINUS
%token TIMES
%token EQUAL
%token IF THEN ELSE
%token LAMBDA LARROW
%token LPAREN RPAREN
%token LET IN
%token SEMISEMI
/* %token EOF */

/* Associativity of operators */
%left PLUS MINUS
%left TIMES
%nonassoc LARROW
%nonassoc LET
%nonassoc EQUAL
%nonassoc IN
%nonassoc ELSE


%start toplevel
%type <Types.expr> toplevel

%%

toplevel:
  | d = ast_expr SEMISEMI
    { d }

ast_expr:
  | LPAREN e = ast_expr RPAREN
    { e }
  | MINUS n = INTEGER
    { Integer (-n)}
  | n = INTEGER
    { Integer n }
  | TRUE
    { Boolean true }
  | FALSE
    { Boolean false }
  | e1 = ast_expr PLUS e2 = ast_expr
    { Sum (e1, e2) }
  | e1 = ast_expr MINUS e2 = ast_expr
    { Sub (e1, e2) }
  | e1 = ast_expr TIMES e2 = ast_expr
    { Mult (e1, e2) }
  | e1 = ast_expr EQUAL e2 = ast_expr
    { Eq (e1, e2) }
  | e1 = ast_expr AND e2 = ast_expr
    { And (e1, e2)}
  | e1 = ast_expr OR e2 = ast_expr
    { And (e1, e2)}
  | NOT e1 = ast_expr
    { Not e1}
  | var = SYMBOL
    { Symbol var }
  | IF g = ast_expr THEN b = ast_expr ELSE e = ast_expr
    { IfThenElse (g, b, e)}
  | LAMBDA params = SYMBOL+ LARROW body = ast_expr
    { Lambda (params, body) }
  | f = SYMBOL params = ast_expr+
    { Apply (Symbol f, params)}
  | f = delimited(LPAREN, ast_expr, RPAREN) params = ast_expr+
    { Apply (f, params)}
  | LET name = SYMBOL EQUAL value = ast_expr IN body = ast_expr
    { Let (name, value, body) }


%%