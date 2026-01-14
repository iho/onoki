%{
  open Ast
%}

%token <int> INT
%token <float> FLOAT
%token <string> STRING IDENT
%token LET REC IN FUN IF THEN ELSE MATCH WITH TYPE MODULE OPEN EXTERNAL
%token TRUE FALSE AND OR NOT
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token PLUS MINUS STAR SLASH MOD
%token EQ NE LT LE GT GE
%token ARROW CONS DOT SEMICOLON COMMA PIPE UNDERSCORE
%token EOF

%left OR
%left AND
%left EQ NE LT LE GT GE
%right CONS
%left PLUS MINUS
%left STAR SLASH MOD
%nonassoc NOT
%nonassoc UMINUS

%start <Ast.program> prog
%start <Ast.expr> expr_top

%%

prog:
  | decls = list(decl) EOF { decls }

decl:
  | LET name = IDENT EQ e = expr 
      { DeclLet (name, e) }
  | LET REC name = IDENT EQ e = expr 
      { DeclLetRec (name, e) }
  | TYPE name = IDENT EQ ty = type_expr
      { DeclType (name, ty) }
  | EXTERNAL name = IDENT CONS ty = type_expr
      { DeclExtern (name, ty) }

expr_top:
  | e = expr EOF { e }

expr:
  | i = INT 
      { Int i }
  | f = FLOAT 
      { Float f }
  | s = STRING 
      { String s }
  | TRUE 
      { Bool true }
  | FALSE 
      { Bool false }
  | x = IDENT 
      { Var x }
  | LPAREN e = expr RPAREN 
      { e }
  | LPAREN RPAREN
      { Tuple [] }
  | e1 = expr PLUS e2 = expr 
      { BinOp (Add, e1, e2) }
  | e1 = expr MINUS e2 = expr 
      { BinOp (Sub, e1, e2) }
  | e1 = expr STAR e2 = expr 
      { BinOp (Mul, e1, e2) }
  | e1 = expr SLASH e2 = expr 
      { BinOp (Div, e1, e2) }
  | e1 = expr MOD e2 = expr 
      { BinOp (Mod, e1, e2) }
  | e1 = expr EQ e2 = expr 
      { BinOp (Eq, e1, e2) }
  | e1 = expr NE e2 = expr 
      { BinOp (Ne, e1, e2) }
  | e1 = expr LT e2 = expr 
      { BinOp (Lt, e1, e2) }
  | e1 = expr LE e2 = expr 
      { BinOp (Le, e1, e2) }
  | e1 = expr GT e2 = expr 
      { BinOp (Gt, e1, e2) }
  | e1 = expr GE e2 = expr 
      { BinOp (Ge, e1, e2) }
  | e1 = expr AND e2 = expr 
      { BinOp (And, e1, e2) }
  | e1 = expr OR e2 = expr 
      { BinOp (Or, e1, e2) }
  | NOT e = expr 
      { UnOp (Not, e) }
  | MINUS e = expr %prec UMINUS
      { UnOp (Neg, e) }
  | e1 = expr CONS e2 = expr 
      { Cons (e1, e2) }
  | FUN params = nonempty_list(IDENT) ARROW body = expr 
      { List.fold_right (fun p acc -> Fun (p, acc)) params body }
  | e1 = expr e2 = simple_expr 
      { App (e1, e2) }
  | LET name = IDENT EQ e1 = expr IN e2 = expr 
      { Let (name, e1, e2) }
  | LET REC name = IDENT EQ e1 = expr IN e2 = expr 
      { LetRec (name, e1, e2) }
  | IF cond = expr THEN then_ = expr ELSE else_ = expr 
      { If (cond, then_, else_) }
  | MATCH scrutinee = expr WITH cases = match_cases 
      { Match (scrutinee, cases) }
  | LBRACKET elems = separated_list(SEMICOLON, expr) RBRACKET 
      { List elems }
  | LPAREN es = separated_list(COMMA, expr) RPAREN 
      { match es with
        | [] -> Tuple []
        | [e] -> e
        | _ -> Tuple es }
  | e = expr DOT field = IDENT 
      { Field (e, field) }
  | LBRACE fields = separated_list(SEMICOLON, field_binding) RBRACE 
      { Record fields }

simple_expr:
  | i = INT { Int i }
  | f = FLOAT { Float f }
  | s = STRING { String s }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | x = IDENT { Var x }
  | LPAREN e = expr RPAREN { e }
  | LBRACKET elems = separated_list(SEMICOLON, expr) RBRACKET { List elems }

field_binding:
  | name = IDENT EQ e = expr { (name, e) }

match_cases:
  | PIPE? cases = separated_nonempty_list(PIPE, match_case) { cases }

match_case:
  | pat = pattern ARROW e = expr { (pat, e) }

pattern:
  | i = INT 
      { PatInt i }
  | x = IDENT 
      { PatVar x }
  | UNDERSCORE 
      { PatWildcard }
  | LPAREN pats = separated_list(COMMA, pattern) RPAREN 
      { match pats with
        | [] -> PatWildcard
        | [p] -> p
        | _ -> PatTuple pats }
  | p1 = pattern CONS p2 = pattern 
      { PatCons (p1, p2) }
  | LBRACE fields = separated_list(SEMICOLON, pattern_field) RBRACE 
      { PatRecord fields }

pattern_field:
  | name = IDENT EQ pat = pattern { (name, pat) }

type_expr:
  | IDENT { TyVar "TODO" }
