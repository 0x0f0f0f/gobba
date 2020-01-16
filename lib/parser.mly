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

%start file
%type <Types.command list> file

%start toplevel
%type <Types.command> toplevel

%%



file:
  | EOF
    { [] }
  | e = ast_expr EOF
    { (Expr e) :: [] }
  | e = ast_expr SEMISEMI lst = file
    { Expr(e) :: lst }
  | d = def SEMISEMI lst = file
    { (Def d) :: lst }
  | d = def EOF
    { (Def d) :: [] }
  | d = directive lst = file
  { (Directive d)::lst }

toplevel:
  | d = directive
  { Directive d }
  | l = separated_list(SEMI, ast_expr) SEMISEMI? EOF
  { Expr(Sequence(l)) }
  | d = def SEMISEMI? EOF
  { Def d }
  | d = ast_expr SEMISEMI? EOF
  { Expr d }

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
    | "#import" -> Includefileasmodule (a, None)
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
  | e = ast_expr CONS ls = ast_expr
  { Cons (e, ls) }
  | NOT e1 = ast_expr
  { Not e1}
  | e1 = ast_expr ATSIGN e2 = ast_expr
  { Apply(Apply(Symbol "nth", e2), e1) }
  | e1 = ast_expr CONCAT e2 = ast_expr
  { Concat (e1, e2) }
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
  | d = def IN body = ast_expr
  { Let (d, body) }
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
  | e = ast_simple_expr COLON s = SYMBOL
  { Apply(Apply(Symbol "getkey", String(s)), e) }
  | PURE e = ast_expr
  { Purity (Pure, e)}
  | IMPURE e = ast_expr
  { Purity (Impure, e)}
  | l = delimited(LSQUARE, separated_list(SEMI, ast_expr), RSQUARE)
  { List l }
  | l = delimited(LBRACKET, separated_list(SEMI, assignment), RBRACKET)
  { Dict (List.map (fun (_, k, v) -> (k,v)) l) }
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