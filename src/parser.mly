%{
  open Ast
%}

%token <int> INT
%token <float> FLOAT
%token <string> STRING LIDENT UIDENT
%token LET REC IN FUN IF THEN ELSE MATCH WITH TYPE MODULE OPEN EXTERNAL INCLUDE
%token TRUE FALSE AND OR NOT
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token PLUS MINUS STAR SLASH MOD
%token EQ NE LT LE GT GE
%token ARROW CONS COLON DOT SEMICOLON COMMA PIPE UNDERSCORE
%token HAT
%token OF  (* New token for Constructor of Type *)
%token EOF

%left OR
%left AND
%left EQ NE LT LE GT GE
%right CONS
%right HAT
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
  | LET name = LIDENT EQ e = expr 
      { DeclLet (name, e) }
  | LET REC name = LIDENT EQ e = expr 
      { DeclLetRec (name, e) }
  | TYPE name = LIDENT EQ ty = type_expr
      { DeclType (name, ty) }
  | TYPE name = LIDENT EQ constrs = type_variants
      { DeclType (name, TyVariant (name, constrs)) }
  | EXTERNAL name = LIDENT COLON ty = type_expr EQ impl = STRING
      { DeclExtern (name, ty, impl) }
  | MODULE name = UIDENT EQ LBRACE decls = list(decl) RBRACE
      { DeclModule (name, decls) }
  | INCLUDE path = STRING
      { DeclInclude path }

type_variants:
  | PIPE? constrs = separated_nonempty_list(PIPE, constructor_decl) { constrs }

constructor_decl:
  | name = UIDENT { (name, None) }
  | name = UIDENT OF t = type_expr { (name, Some t) }

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
  | x = LIDENT 
      { Var x }
  | c = UIDENT
      { Variant(c, None) }

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
  | e1 = expr HAT e2 = expr 
      { BinOp (Concat, e1, e2) }
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
  | FUN params = nonempty_list(LIDENT) ARROW body = expr 
      { List.fold_right (fun p acc -> Fun (p, acc)) params body }
  | c = UIDENT arg = simple_expr 
      { Variant (c, Some arg) }
  | e1 = expr e2 = simple_expr 
      { App (e1, e2) }
  | LET name = LIDENT EQ e1 = expr IN e2 = expr 
      { Let (name, e1, e2) }
  | LET REC name = LIDENT EQ e1 = expr IN e2 = expr 
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
  | e = expr DOT field = LIDENT 
      { Field (e, field, ref None) }
  | LBRACE fields = separated_list(SEMICOLON, field_binding) RBRACE 
      { Record (ref None, fields) }

simple_expr:
  | i = INT { Int i }
  | f = FLOAT { Float f }
  | s = STRING { String s }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | x = LIDENT { Var x }
  | c = UIDENT { Variant(c, None) }
  | LPAREN es = separated_list(COMMA, expr) RPAREN 
      { match es with
        | [] -> Tuple []
        | [e] -> e
        | _ -> Tuple es }
  | LBRACKET elems = separated_list(SEMICOLON, expr) RBRACKET { List elems }

field_binding:
  | name = LIDENT EQ e = expr { (name, e) }

match_cases:
  | PIPE? cases = separated_nonempty_list(PIPE, match_case) { cases }

match_case:
  | pat = pattern ARROW e = expr { (pat, e) }

pattern:
  | p1 = constructor_pattern CONS p2 = pattern 
      { PatCons (p1, p2) }
  | p = constructor_pattern 
      { p }

constructor_pattern:
  | c = UIDENT p = simple_pattern 
      { PatVariant (c, Some p) }
  | p = simple_pattern 
      { p }

simple_pattern:
  | i = INT 
      { PatInt i }
  | x = LIDENT 
      { PatVar x }
  | c = UIDENT
      { PatVariant (c, None) }
  | UNDERSCORE 
      { PatWildcard }
  | LPAREN pats = separated_list(COMMA, pattern) RPAREN 
      { match pats with
        | [] -> PatWildcard
        | [p] -> p
        | _ -> PatTuple pats }
  | LBRACKET pats = separated_list(SEMICOLON, pattern) RBRACKET
      { PatList pats }
  | LBRACE fields = separated_list(SEMICOLON, pattern_field) RBRACE 
      { PatRecord fields }

pattern_field:
  | name = LIDENT EQ pat = pattern { (name, pat) }

type_expr:
  | t1 = type_expr ARROW t2 = type_expr 
      { TyFun (t1, t2) }
  | t = type_simple 
      { t }

type_simple:
  | name = LIDENT 
      { match name with
        | "int" -> TyInt
        | "float" -> TyFloat
        | "bool" -> TyBool
        | "string" -> TyString
        | "unit" -> TyTuple []
        | _ -> TyVar name }
  | type_simple LIDENT (* list type application *)
      { match $2 with
        | "list" -> TyList $1
        | _ -> failwith "Only list type constructor supported" }
  | LPAREN t = type_expr RPAREN 
      { t }
  | LPAREN ts = separated_nonempty_list(COMMA, type_expr) RPAREN
      { TyTuple ts }
  | LBRACE fields = separated_list(SEMICOLON, type_field) RBRACE
      { TyRecord (None, fields) }

type_field:
  | name = LIDENT COLON t = type_expr { (name, t) }

