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
%token NOT
%token LAND
%token OR
%token CONCATSTR
%token CONCATLST
%token PLUS
%token MINUS
%token TIMES
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
%token HASH
%token EOF

/* Associativity of operators */

%left PIPE
%nonassoc LAMBDA LARROW
%nonassoc IF THEN ELSE
%left LAND OR
%left PLUS MINUS
%left TIMES
%left DIV
%left EQUAL DIFFER GREATER GREATEREQUAL LESS LESSEQUAL

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

toplevel:
  | HASH d = directive
  { Directive d }
  | l = separated_list(SEMI, ast_expr) SEMISEMI? EOF
  { Expr(Sequence(l)) }
  | d = def SEMISEMI? EOF
  { d }
  | d = ast_expr SEMISEMI? EOF
  { Expr d }

directive:
  | PURE { Setpurity Pure }
  | IMPURE { Setpurity Impure }
  | s = SYMBOL a = STRING
  { match s with
    | "include" -> Includefile a
    | _ -> failwith "unknown directive" }
  | s = SYMBOL i = INTEGER
  { match s with
    | "verbose" -> Setverbose i
    | _ -> failwith "unknown directive" }

assignment:
  | name = SYMBOL EQUAL value = ast_expr
  { (false, name, value) }
  | LAZY name = SYMBOL EQUAL value = ast_expr
  { (true, name, value) }

dict_value:
  | key = SYMBOL COLON value = ast_expr
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
  | e1 = ast_expr CONCATLST e2 = ast_expr
  { ConcatLists (e1, e2) }
  | e1 = ast_expr CONCATSTR e2 = ast_expr
  { ConcatStrings (e1, e2) }
  | e1 = ast_expr LAND e2 = ast_expr
  { And (e1, e2)}
  | e1 = ast_expr OR e2 = ast_expr
  { Or (e1, e2)}
  | e1 = ast_expr PLUS e2 = ast_expr
  { Plus(e1, e2) }
  | e1 = ast_expr MINUS e2 = ast_expr
  { Sub (e1, e2) }
  | e1 = ast_expr TIMES e2 = ast_expr
  { Mult (e1, e2) }
  | e1 = ast_expr DIV e2 = ast_expr
  { Div (e1, e2) }
  | e1 = ast_expr EQUAL e2 = ast_expr
  { Eq (e1, e2) }
  | e1 = ast_expr DIFFER e2 = ast_expr
  { Not(Eq (e1, e2)) }
  | e1 = ast_expr GREATER e2 = ast_expr
  { Gt (e1, e2) }
  | e1 = ast_expr LESS e2 = ast_expr
  { Lt (e1, e2) }
  | e1 = ast_expr GREATEREQUAL e2 = ast_expr
  { Ge (e1, e2) }
  | e1 = ast_expr LESSEQUAL e2 = ast_expr
  { Le (e1, e2) }
  | IF g = ast_expr THEN b = ast_expr ELSE e = ast_expr
  { IfThenElse (g, b, e)}
  | LET a = separated_list(AND, assignment) IN body = ast_expr
  { Let (a, body) }
  | LAMBDA params = SYMBOL+ LARROW body = ast_expr
  { lambda_from_paramlist params body }
  | e1 = ast_expr COMPOSE e2 = ast_expr
  { Compose(e1, e2) }
  | e1 = ast_expr PIPE  e2 = ast_expr
  { Compose(e2, e1) }

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
  | LPAREN e = ast_expr RPAREN
  { e }
  | PURE e = ast_expr
  { Purity (Pure, e)}
  | IMPURE e = ast_expr
  { Purity (Impure, e)}
  | l = delimited(LSQUARE, separated_list(SEMI, ast_expr), RSQUARE)
  { List l }
  | l = delimited(LBRACKET, separated_list(COMMA, dict_value), RBRACKET)
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