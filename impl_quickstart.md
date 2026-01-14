# Quick-Start: Skeleton Implementation

This guide helps you begin implementing the architecture in practice.

## Project Scaffold

```bash
# Create project structure
mkdir ocaml-jvm-lang
cd ocaml-jvm-lang
opam switch create . ocaml-base-compiler.5.1.0
eval $(opam env)

# Install dependencies
opam install dune menhir ocamllex yojson ppx_deriving

# Create directory tree
mkdir -p {src,stdlib,tests,bin}
touch dune-project
```

## 1. Minimal dune-project

```lisp
(lang dune 3.7)
(name ocaml_jvm)
(version 0.1.0)
```

## 2. Start: src/ast.ml

```ocaml
(* Core AST definitions *)

type ty =
  | TyInt
  | TyFloat
  | TyString
  | TyBool
  | TyVar of string
  | TyFun of ty * ty
  | TyTuple of ty list
  [@@deriving sexp]

type binop = Add | Sub | Mul | Div | Mod | Eq | Ne | Lt | Le | Gt | Ge | And | Or
  [@@deriving sexp]

type expr =
  | Int of int
  | Float of float
  | String of string
  | Bool of bool
  | Var of string
  | BinOp of binop * expr * expr
  | If of expr * expr * expr
  | Fun of string * expr
  | App of expr * expr
  | Let of string * expr * expr
  | LetRec of string * expr * expr
  [@@deriving sexp]

type pattern =
  | PatInt of int
  | PatVar of string
  | PatWildcard
  [@@deriving sexp]

type program = expr
```

## 3. src/lexer.mll

```ocaml
{
  type token =
    | INT of int | FLOAT of float | STRING of string | IDENT of string
    | LET | REC | IN | FUN | IF | THEN | ELSE
    | LPAREN | RPAREN | DOT | SEMICOLON | COMMA | ARROW
    | PLUS | MINUS | STAR | SLASH | EQ | LT | GT | LE | GE | NE
    | COLONEQUAL | EOF
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_']
let alnum = alpha | digit | '\''

rule token = parse
  | [' ' '\t' '\n'] { token lexbuf }
  | "(*" { comment lexbuf; token lexbuf }
  | digit+ as i { INT (int_of_string i) }
  | digit+ '.' digit+ as f { FLOAT (float_of_string f) }
  | '"' ([^'"']* as s) '"' { STRING s }
  | "let" { LET }
  | "rec" { REC }
  | "in" { IN }
  | "fun" { FUN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | alpha alnum* as id { IDENT id }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { STAR }
  | '/' { SLASH }
  | '=' { EQ }
  | "<=" { LE }
  | ">=" { GE }
  | '<' { LT }
  | '>' { GT }
  | "<>" { NE }
  | "->" { ARROW }
  | eof { EOF }

and comment = parse
  | "*)" { () }
  | _ { comment lexbuf }
```

## 4. src/parser.mly

```ocaml
%token <int> INT
%token <float> FLOAT
%token <string> STRING IDENT
%token LET REC IN FUN IF THEN ELSE
%token LPAREN RPAREN ARROW
%token PLUS MINUS STAR SLASH EQ LT GT LE GE NE
%token EOF

%left PLUS MINUS
%left STAR SLASH

%%

prog:
  | expr EOF { $1 }

expr:
  | INT { Ast.Int $1 }
  | FLOAT { Ast.Float $1 }
  | STRING { Ast.String $1 }
  | IDENT { Ast.Var $1 }
  | LPAREN expr RPAREN { $2 }
  | expr PLUS expr { Ast.BinOp (Ast.Add, $1, $3) }
  | expr MINUS expr { Ast.BinOp (Ast.Sub, $1, $3) }
  | expr STAR expr { Ast.BinOp (Ast.Mul, $1, $3) }
  | expr SLASH expr { Ast.BinOp (Ast.Div, $1, $3) }
  | expr EQ expr { Ast.BinOp (Ast.Eq, $1, $3) }
  | expr LT expr { Ast.BinOp (Ast.Lt, $1, $3) }
  | FUN IDENT ARROW expr { Ast.Fun ($2, $4) }
  | expr expr %prec STAR { Ast.App ($1, $2) }
  | LET IDENT EQ expr IN expr { Ast.Let ($2, $4, $6) }
  | LET REC IDENT EQ expr IN expr { Ast.LetRec ($3, $5, $7) }
  | IF expr THEN expr ELSE expr { Ast.If ($2, $4, $6) }
```

## 5. src/typing.ml (skeleton)

```ocaml
open Ast

type env = (string * ty) list

let rec infer (env : env) (expr : expr) : ty option =
  match expr with
  | Int _ -> Some TyInt
  | Float _ -> Some TyFloat
  | String _ -> Some TyString
  | Bool _ -> Some TyBool
  | Var x -> List.assoc_opt x env
  | BinOp (Add, e1, e2) -> (
      match infer env e1, infer env e2 with
      | Some TyInt, Some TyInt -> Some TyInt
      | Some TyFloat, Some TyFloat -> Some TyFloat
      | _ -> None)
  | Fun (x, body) ->
      let env' = (x, TyVar "a") :: env in
      (match infer env' body with
       | Some ret_ty -> Some (TyFun (TyVar "a", ret_ty))
       | None -> None)
  | App (f, arg) -> (
      match infer env f, infer env arg with
      | Some (TyFun (arg_ty, ret_ty)), Some arg_ty' 
        when arg_ty = arg_ty' -> Some ret_ty
      | _ -> None)
  | Let (x, e1, e2) -> (
      match infer env e1 with
      | Some ty1 ->
          let env' = (x, ty1) :: env in
          infer env' e2
      | None -> None)
  | If (cond, then_, else_) -> (
      match infer env cond with
      | Some TyBool -> (
          match infer env then_, infer env else_ with
          | Some t1, Some t2 when t1 = t2 -> Some t1
          | _ -> None)
      | _ -> None)
  | _ -> None
```

## 6. src/lambda.ml (skeleton)

```ocaml
open Ast

(* Simplified Lambda IR *)
type lambda =
  | LConst of [ `Int of int | `Float of float | `String of string ]
  | LVar of string
  | LFun of string list * lambda
  | LApp of lambda * lambda list
  | LLet of string * lambda * lambda
  | LIf of lambda * lambda * lambda

let rec lower_expr : expr -> lambda = function
  | Int i -> LConst (`Int i)
  | Float f -> LConst (`Float f)
  | String s -> LConst (`String s)
  | Var x -> LVar x
  | Fun (x, body) -> LFun ([x], lower_expr body)
  | App (f, arg) -> LApp (lower_expr f, [lower_expr arg])
  | Let (x, e1, e2) -> LLet (x, lower_expr e1, lower_expr e2)
  | If (c, t, e) -> LIf (lower_expr c, lower_expr t, lower_expr e)
  | _ -> failwith "TODO"

let lower_program : program -> lambda = lower_expr
```

## 7. src/dune

```lisp
(library
 (name ocaml_jvm)
 (modules ast typing lambda jvmgen)
 (libraries yojson ppx_deriving.runtime))

(ocamllex lexer)
(menhir (modules parser))
```

## 8. bin/main.ml

```ocaml
open Ocaml_jvm

let () =
  let filename = Sys.argv.(1) in
  let ic = open_in filename in
  let source = really_input_string ic (in_channel_length ic) in
  close_in ic;

  (* Lex & Parse *)
  let lexbuf = Lexing.from_string source in
  try
    let ast = Parser.prog Lexer.token lexbuf in
    Printf.printf "Parsed: %s\n" (Sexplib0.Sexp.to_string_hum (Ast.sexp_of_expr ast));

    (* Type check *)
    (match Typing.infer [] ast with
     | Some ty -> Printf.printf "Type: %s\n" (Sexplib0.Sexp.to_string_hum (Ast.sexp_of_ty ty))
     | None -> Printf.printf "Type error!\n");

    (* Lower to Lambda *)
    let lam = Lambda.lower_expr ast in
    Printf.printf "Lambda: %s\n" (show_lambda lam)

  with Parser.Error ->
    Printf.eprintf "Parse error at line %d\n" lexbuf.lex_curr_p.pos_lnum
```

## 9. Build & Test

```bash
# Build
dune build

# Run on example
echo 'let x = 5 in let y = 3 in x' > test.ml
dune exec bin/main.exe test.ml
```

## Next Steps

1. **Complete Typing Module**: Implement proper unification algorithm
2. **Implement JVM Code Generation**: Use ASM library bindings to write `.class` files
3. **Add Standard Library**: Arithmetic, list ops, I/O
4. **Test Suite**: Unit tests for each stage
5. **Performance**: Add inline, dead-code elimination, TCO optimization passes

## Key References

- [OCaml Compiler Sources](https://github.com/ocaml/ocaml)
- [Real World OCaml - Compiler Backend](https://dev.realworldocaml.org/)
- [Menhir Documentation](http://gallium.inria.fr/~fpottier/menhir/)
- [JVM Bytecode Spec](https://docs.oracle.com/javase/specs/jvms/se21/html/)
- [ASM Framework Docs](https://asm.ow2.io/)

## Architecture Highlights

**Pattern Matching for Compiler**: Use OCaml's pattern matching on the AST to handle all language constructs uniformly

**Modular Type Checking**: Environment-based typing makes it easy to extend with new primitives

**Lambda Lowering**: Clean separation between high-level AST and low-level IR for codegen

**JVM as Target**: Bytecode stability means your compiler can evolve independently from platform changes
