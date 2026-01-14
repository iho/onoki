# Architecture: Functional OCaml-like Language for JVM in OCaml

## Executive Summary

A multi-stage compiler architecture implementing a functional ML-like language targeting the JVM, written in OCaml. The design leverages OCaml's algebraic data types, pattern matching, and module system for clean compiler implementation, while generating optimized JVM bytecode.

---

## 1. Core Architecture Overview

### Compilation Pipeline

```
Source Code (.ml)
    ↓
[Lexer] (menhir/ocamllex)
    ↓
Tokens
    ↓
[Parser] (menhir)
    ↓
AST (Abstract Syntax Tree)
    ↓
[Type Checker] (bidirectional type inference + unification)
    ↓
Typed AST
    ↓
[Lambda Form Lowering] (eliminate type info, optimize patterns)
    ↓
Lambda IR
    ↓
[CPS Conversion] (optional, for advanced tail-call optimization)
    ↓
Administrative Normal Form (ANF)
    ↓
[JVM Code Generation]
    ↓
JVM Bytecode (.class files)
    ↓
[JVM Execution]
```

### Key Design Decisions

1. **Bytecode Input Model**: Generate JVM bytecode directly using ASM (ObjectWeb Assembly) Java library via runtime bytecode generation (similar to js_of_ocaml approach of targeting a stable VM bytecode)
2. **Type System**: Full ML-family type inference with generics support
3. **Memory Model**: Leverage JVM's GC; map OCaml values to Java objects
4. **FFI Strategy**: Seamless Java interop for std library and system calls

---

## 2. Lexer & Parser Layer

### Tool Selection
- **ocamllex**: Pattern-based lexical analysis
- **menhir**: GLR parser generator (handles complex grammars better than ocamlyacc)

### Lexer (`src/lexer.mll`)

```ocaml
(* Token definitions *)
type token =
  | INT of int
  | FLOAT of float
  | STRING of string
  | IDENT of string
  | LET | REC | IN | AND
  | FUN | MATCH | WITH | IF | THEN | ELSE
  | TYPE | MODULE | OPEN
  | LPAREN | RPAREN | LBRACKET | RBRACKET | LBRACE | RBRACE
  | DOT | SEMICOLON | COMMA | PIPE | ARROW
  | PLUS | MINUS | STAR | SLASH | MOD
  | EQ | NE | LT | LE | GT | GE
  | AND_OP | OR_OP | NOT_OP
  | ASSIGN | COLONEQUAL
  | EOF

(* Lexer rules *)
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
  | "fun" { FUN }
  | "match" { MATCH }
  | alpha alnum* as id { IDENT id }
  | eof { EOF }
  | _ as c { failwith (Printf.sprintf "Unexpected char: %c" c) }
```

### Parser (`src/parser.mly`)

```ocaml
%token <int> INT
%token <float> FLOAT
%token <string> STRING IDENT
%token LET REC IN FUN MATCH WITH IF THEN ELSE TYPE MODULE
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token DOT SEMICOLON COMMA PIPE ARROW PLUS MINUS STAR SLASH
%token EOF

%left PLUS MINUS
%left STAR SLASH

%%

prog:
  | expr EOF { $1 }

expr:
  | INT { Expr.Int $1 }
  | FLOAT { Expr.Float $1 }
  | STRING { Expr.String $1 }
  | IDENT { Expr.Var $1 }
  | LPAREN expr RPAREN { $2 }
  | expr PLUS expr { Expr.BinOp (Expr.Add, $1, $3) }
  | expr STAR expr { Expr.BinOp (Expr.Mul, $1, $3) }
  | FUN IDENT ARROW expr { Expr.Fun ($2, $4) }
  | LET IDENT EQ expr IN expr { Expr.Let ($2, $4, $6) }
  | LET REC IDENT EQ expr IN expr { Expr.LetRec ($3, $5, $7) }
  | IF expr THEN expr ELSE expr { Expr.If ($2, $4, $6) }
  | MATCH expr WITH match_cases { Expr.Match ($2, $4) }

match_cases:
  | pattern ARROW expr { [($1, $3)] }
  | pattern ARROW expr PIPE match_cases { ($1, $3) :: $5 }
```

---

## 3. Abstract Syntax Tree (AST)

### Module `src/ast.ml`

```ocaml
(* Type Definitions *)
type ty =
  | TyInt
  | TyFloat
  | TyString
  | TyBool
  | TyVar of string  (* type variable for generics *)
  | TyFun of ty * ty  (* function type: arg -> result *)
  | TyTuple of ty list
  | TyList of ty
  | TyVariant of string * (string * ty option) list  (* constructor list *)
  | TyRecord of (string * ty) list
  | TyApp of string * ty list  (* e.g., list<int> *)

(* Expressions *)
type expr =
  | Int of int
  | Float of float
  | String of string
  | Bool of bool
  | Var of string
  | BinOp of binop * expr * expr
  | UnOp of unop * expr
  | If of expr * expr * expr
  | Fun of string * expr  (* lambda: fun x -> body *)
  | App of expr * expr  (* function application *)
  | Let of string * expr * expr  (* let x = e1 in e2 *)
  | LetRec of string * expr * expr  (* let rec f = e1 in e2 *)
  | Match of expr * (pattern * expr) list
  | Tuple of expr list
  | List of expr list
  | Cons of expr * expr  (* head :: tail *)
  | Variant of string * expr option  (* Some x, None *)
  | Record of (string * expr) list
  | Field of expr * string  (* r.field *)
  | Seq of expr list  (* e1; e2; e3 *)

and binop = Add | Sub | Mul | Div | Mod | Eq | Ne | Lt | Le | Gt | Ge | And | Or

and unop = Not | Neg

(* Patterns *)
type pattern =
  | PatInt of int
  | PatVar of string
  | PatWildcard
  | PatTuple of pattern list
  | PatCons of pattern * pattern
  | PatVariant of string * pattern option
  | PatRecord of (string * pattern) list

(* Top-level declarations *)
type decl =
  | DeclLet of string * expr
  | DeclLetRec of string * expr
  | DeclType of string * ty
  | DeclModule of string * decl list
  | DeclExtern of string * ty  (* external FFI *)

type program = decl list
```

---

## 4. Type Checking & Inference

### Module `src/typing.ml`

**Key Features:**
- Hindley-Milner type inference with rank-1 polymorphism
- Unification algorithm
- Occurs check to prevent infinite types

```ocaml
module Typing = struct
  type subst = (string * ty) list

  type env = (string * ty scheme) list
  and ty_scheme = Forall of string list * ty  (* quantified type variables *)

  (* Unification *)
  let rec unify (t1 : ty) (t2 : ty) : subst option =
    match t1, t2 with
    | TyInt, TyInt | TyFloat, TyFloat | TyString, TyString | TyBool, TyBool ->
        Some []
    | TyVar v1, TyVar v2 when v1 = v2 -> Some []
    | TyVar v, t | t, TyVar v ->
        if occurs_check v t then None else Some [(v, t)]
    | TyFun (a1, r1), TyFun (a2, r2) ->
        (match unify a1 a2 with
         | None -> None
         | Some s1 ->
             let r1' = apply_subst s1 r1 in
             let r2' = apply_subst s1 r2 in
             (match unify r1' r2' with
              | None -> None
              | Some s2 -> Some (s1 @ s2)))
    | TyTuple ts1, TyTuple ts2 when List.length ts1 = List.length ts2 ->
        unify_lists ts1 ts2
    | _, _ -> None

  (* Bidirectional type checking *)
  let rec infer (env : env) (expr : expr) : (ty * subst) option =
    match expr with
    | Int _ -> Some (TyInt, [])
    | Float _ -> Some (TyFloat, [])
    | String _ -> Some (TyString, [])
    | Bool _ -> Some (TyBool, [])
    | Var x -> (match lookup env x with
                | Some (Forall (tvars, ty)) -> Some (ty, [])
                | None -> None)
    | BinOp (Add, e1, e2) ->
        (match infer env e1, infer env e2 with
         | Some (TyInt, s1), Some (TyInt, s2) -> Some (TyInt, s1 @ s2)
         | Some (TyFloat, s1), Some (TyFloat, s2) -> Some (TyFloat, s1 @ s2)
         | _ -> None)
    | Fun (x, body) ->
        let env' = (x, Forall ([], TyVar (fresh_var ()))) :: env in
        (match infer env' body with
         | Some (ret_ty, subst) ->
             let arg_ty = apply_subst subst (TyVar x) in
             Some (TyFun (arg_ty, ret_ty), subst)
         | None -> None)
    | App (f, arg) ->
        (match infer env f, infer env arg with
         | Some (TyFun (arg_ty, ret_ty), s1), Some (arg_ty', s2) ->
             (match unify arg_ty arg_ty' with
              | Some s3 -> Some (apply_subst s3 ret_ty, s1 @ s2 @ s3)
              | None -> None)
         | _ -> None)
    | Let (x, e1, e2) ->
        (match infer env e1 with
         | Some (ty1, s1) ->
             let scheme = generalize env (apply_subst s1 ty1) in
             let env' = (x, scheme) :: env in
             infer env' e2
         | None -> None)
    | _ -> failwith "TODO: other expression types"
end
```

---

## 5. Lambda Form (Intermediate Representation)

### Module `src/lambda.ml`

**Purpose:** Eliminate type information, optimize pattern matching into decision trees, prepare for code generation.

```ocaml
(* Lambda Form - simplified, untyped IR *)
type lambda =
  | Lconst of constant
  | Lvar of string
  | Lfunction of string list * lambda  (* closure: args -> body *)
  | Lapply of lambda * lambda list  (* tail-recursive application *)
  | Llet of string * lambda * lambda  (* let binding *)
  | Lletrec of (string * lambda) list * lambda  (* recursive bindings *)
  | Lswitch of lambda * (int * lambda) list * lambda option  (* pattern dispatch *)
  | Lif of lambda * lambda * lambda
  | Ltuple of lambda list
  | Lfield of lambda * int  (* tuple projection *)
  | Ltry of lambda * (string * lambda)  (* exception handling *)

and constant =
  | Cint of int
  | Cfloat of float
  | Cstring of string
  | Cbool of bool

(* Lowering from typed AST to Lambda form *)
let rec lower_expr (e : expr) : lambda =
  match e with
  | Int i -> Lconst (Cint i)
  | Float f -> Lconst (Cfloat f)
  | String s -> Lconst (Cstring s)
  | Var x -> Lvar x
  | Fun (x, body) -> Lfunction ([x], lower_expr body)
  | App (f, arg) -> Lapply (lower_expr f, [lower_expr arg])
  | Let (x, e1, e2) -> Llet (x, lower_expr e1, lower_expr e2)
  | LetRec (x, e1, e2) -> Lletrec ([(x, lower_expr e1)], lower_expr e2)
  | If (cond, then_, else_) ->
      Lif (lower_expr cond, lower_expr then_, lower_expr else_)
  | Match (scrutinee, cases) ->
      compile_match (lower_expr scrutinee) cases
  | _ -> failwith "TODO: other expressions"

(* Pattern matching compilation to decision trees *)
and compile_match scrutinee cases : lambda =
  (* Use a simple linear search; production would optimize to jump tables *)
  let rec build_tree = function
    | [] -> failwith "Non-exhaustive pattern match"
    | [(PatWildcard, e)] -> lower_expr e
    | [(PatInt i, e)] ->
        Lif (Lapply (Lvar "eq", [scrutinee; Lconst (Cint i)]),
             lower_expr e,
             failwith "Non-exhaustive")
    | (PatInt i, e) :: rest ->
        Lif (Lapply (Lvar "eq", [scrutinee; Lconst (Cint i)]),
             lower_expr e,
             build_tree rest)
    | _ -> failwith "TODO: variant patterns"
  in
  build_tree cases
```

---

## 6. JVM Code Generation

### Strategy

**Target:** JVM bytecode via runtime generation using ASM-like approach or direct `.class` file writing.

#### Key Mapping: OCaml Values → JVM Types

```
OCaml Type          JVM Type             Representation
────────────────────────────────────────────────────────
int                 J (primitive int)    32-bit signed
float               D (primitive double) 64-bit IEEE
bool                Z (primitive bool)   true/false
string              Ljava/lang/String;   String reference
list<T>             [T or Ljava/lang/Object; Array/LinkedList
tuple<T1,T2>        Record class         generated class
variant             Object + tag byte    boxed + discriminant
closure             FunctionN<...>       interface impl
unit                Ljava/lang/Void;     null
```

### Module `src/jvmgen.ml`

```ocaml
module JvmGen = struct
  (* Track class methods being generated *)
  type gen_state = {
    mutable code: jvm_instr list;
    mutable stack_depth: int;
    mutable local_vars: (string * int) list;  (* var name -> local slot *)
    mutable next_label: int;
  }

  type jvm_instr =
    | LoadInt of int
    | LoadFloat of float
    | LoadVar of int  (* local variable slot *)
    | StoreVar of int
    | BinOp of string  (* "iadd", "dadd", etc. *)
    | IfEq of string  (* label *)
    | Goto of string
    | Label of string
    | InvokeStatic of string * string * string  (* class, method, descriptor *)
    | InvokeVirtual of string * string * string
    | Return
    | Aconst of string  (* null, or java/lang/String constant *)

  (* Generate JVM instructions from Lambda form *)
  let rec emit_lambda (state : gen_state) (lam : lambda) : unit =
    match lam with
    | Lconst (Cint i) ->
        state.code <- LoadInt i :: state.code;
        state.stack_depth <- state.stack_depth + 1
    | Lconst (Cfloat f) ->
        state.code <- LoadFloat f :: state.code;
        state.stack_depth <- state.stack_depth + 1
    | Lvar x ->
        let slot = lookup_var state x in
        state.code <- LoadVar slot :: state.code;
        state.stack_depth <- state.stack_depth + 1
    | Lfunction (params, body) ->
        (* Generate a new FunctionN class implementing Closure interface *)
        let class_name = fresh_closure_class_name () in
        let method_body_state = {
          code = [];
          stack_depth = 0;
          local_vars = List.mapi (fun i p -> (p, i)) params;
          next_label = 0;
        } in
        emit_lambda method_body_state body;
        generate_closure_class class_name params method_body_state;
        (* Push reference to instantiated closure *)
        state.code <- Aconst class_name :: state.code;
        state.stack_depth <- state.stack_depth + 1
    | Lapply (Lvar f, args) ->
        (* Invoke function *)
        emit_lambda state (Lvar f);
        List.iter (emit_lambda state) args;
        let method_descriptor = 
          Printf.sprintf "(%s)%s" 
            (String.concat "" (List.map arg_type args))
            return_type
        in
        state.code <- InvokeVirtual ("java/lang/Object", "apply", method_descriptor) 
                     :: state.code
    | Llet (x, e1, e2) ->
        emit_lambda state e1;
        let slot = alloc_var state x in
        state.code <- StoreVar slot :: state.code;
        emit_lambda state e2
    | Lif (cond, then_, else_) ->
        emit_lambda state cond;
        let else_label = fresh_label state in
        let end_label = fresh_label state in
        state.code <- IfEq else_label :: state.code;
        emit_lambda state then_;
        state.code <- Goto end_label :: state.code;
        state.code <- Label else_label :: state.code;
        emit_lambda state else_;
        state.code <- Label end_label :: state.code
    | _ -> failwith "TODO: other lambda forms"

  (* Generate bytecode class file *)
  let generate_classfile (class_name : string) (state : gen_state) : bytes =
    (* Write JVM class file format:
       - Magic (CAFEBABE)
       - Version
       - Constant pool
       - Access flags
       - This class, super class
       - Interfaces
       - Fields
       - Methods
       - Attributes
    *)
    failwith "TODO: Implement full classfile writer"
    (* Or use asm library via Java interop *)
end
```

#### Alternative: Using Java ASM Library

For production, bind to the ObjectWeb ASM library:

```ocaml
(* src/jvmasm.ml - binding to Java ASM via JNI or direct Java calls *)
external create_class_writer : unit -> int = "create_class_writer"
external write_method : int -> string -> string -> bytes -> unit = "write_method"
external emit_instruction : int -> string -> unit = "emit_instruction"
external to_bytecode : int -> bytes = "class_to_bytecode"
```

---

## 7. Runtime System

### Module `src/runtime.ml`

```ocaml
(* Runtime type for closures and values *)
type value =
  | VInt of int
  | VFloat of float
  | VString of string
  | VBool of bool
  | VUnit
  | VTuple of value array
  | VList of value list
  | VVariant of string * value option
  | VClosure of closure

and closure = {
  code: int;  (* function pointer or bytecode offset *)
  env: value array;  (* captured variables *)
}

(* Java interop stubs *)
external invoke_java_method : string -> string -> string -> value array -> value
  = "invoke_java_method"

(* Runtime built-ins *)
let builtin_add x y =
  match x, y with
  | VInt a, VInt b -> VInt (a + b)
  | VFloat a, VFloat b -> VFloat (a +. b)
  | _ -> failwith "Type error in +"

let builtin_print v =
  let s = match v with
    | VInt i -> string_of_int i
    | VFloat f -> string_of_float f
    | VString s -> s
    | VBool b -> if b then "true" else "false"
    | VUnit -> "()"
    | _ -> "<value>"
  in
  print_endline s;
  VUnit
```

---

## 8. Module System & Compilation Units

### Architecture

```
project/
├── src/
│   ├── main.ml              (* driver *)
│   ├── lexer.mll            (* token generation *)
│   ├── parser.mly           (* parse to AST *)
│   ├── ast.ml               (* AST definitions *)
│   ├── typing.ml            (* type checking & inference *)
│   ├── lambda.ml            (* lambda IR lowering *)
│   ├── jvmgen.ml            (* bytecode generation *)
│   ├── runtime.ml           (* runtime system *)
│   └── dune                 (* build config *)
├── stdlib/
│   ├── Prelude.ml           (* built-in functions *)
│   ├── List.ml              (* list operations *)
│   └── Math.ml              (* math functions *)
├── tests/
│   ├── test_typing.ml
│   ├── test_codegen.ml
│   └── test_examples.ml
└── dune-project
```

### Dune Build Configuration

```lisp
(executable
 (name compiler)
 (libraries str yojson)
 (modules main driver lexer parser ast typing lambda jvmgen runtime))

(ocamllex lexer)
(menhir (modules parser))
```

---

## 9. Advanced Features

### 9.1 Tail-Call Optimization

```ocaml
(* CPS (Continuation-Passing Style) conversion for TCO *)
module CPS = struct
  type cps_term =
    | CpsConst of constant
    | CpsVar of string
    | CpsLam of string * cps_term
    | CpsApp of cps_term * cps_term
    | CpsCall of cps_term * cps_term * cps_term  (* function, arg, continuation *)

  (* Convert lambda form to CPS to identify tail calls *)
  let rec to_cps (lam : lambda) (k : cps_term) : cps_term =
    match lam with
    | Lconst c -> CpsApp (k, CpsConst c)
    | Lvar x -> CpsApp (k, CpsVar x)
    | Lapply (f, args) -> CpsCall (to_cps f (CpsVar "dummy"), 
                                    to_cps (List.hd args) (CpsVar "dummy"), 
                                    k)
    | _ -> failwith "TODO"
end
```

### 9.2 Generics & Parametric Polymorphism

```ocaml
(* Type variables are represented as Java type parameters *)
(* list<'a> generates List<Object> + runtime type tags *)
(* 'a -> 'b generates Function<Object, Object> *)

(* Monomorphization: specialize generic code *)
let monomorphize (class_def : string) (type_args : ty list) : string =
  (* Generate List$Int, List$String, etc. *)
  let suffix = String.concat "_" (List.map type_to_suffix type_args) in
  Printf.sprintf "%s$%s" class_def suffix
```

### 9.3 Exception Handling

```ocaml
(* Map OCaml exceptions to Java Throwable *)
type exc_value = {
  exn_name: string;
  exn_payload: value;
}

(* Try-catch compilation *)
let emit_try_catch state try_body (handler_var, handler_body) =
  let try_label = fresh_label state in
  let handler_label = fresh_label state in
  emit_lambda state try_body;
  (* JVM: TRY try_label ... CATCH handler_label *)
  emit_lambda state handler_body
```

---

## 10. Optimization Passes

### 10.1 Inline Functions

```ocaml
module Optimize = struct
  let rec inline_small_functions (lam : lambda) : lambda =
    match lam with
    | Llet (x, Lfunction ([], body), rest) when term_size body < 10 ->
        (* Substitute function body for calls *)
        let rest' = substitute x body rest in
        inline_small_functions rest'
    | _ -> lam
end
```

### 10.2 Dead Code Elimination

```ocaml
let rec eliminate_dead_code (lam : lambda) : lambda =
  match lam with
  | Llet (x, e1, e2) when not (occurs x e2) ->
      (* x is never used, drop the binding *)
      eliminate_dead_code e2
  | _ -> lam
```

---

## 11. Example: Compiling Factorial

### Source Code
```ocaml
let rec fac n =
  if n <= 1 then 1 else n * fac (n - 1)

let result = fac 5
```

### AST
```
LetRec("fac", 
  Fun("n", 
    If(BinOp(Le, Var "n", Int 1),
       Int 1,
       BinOp(Mul, Var "n", App(Var "fac", BinOp(Sub, Var "n", Int 1))))),
  Let("result", App(Var "fac", Int 5), Var "result"))
```

### Typed AST
```
All type variables unified:
fac : int -> int
result : int
```

### Lambda Form
```
Lletrec([("fac", Lfunction(["n"], 
  Lif(Lapply(Lvar "le", [Lvar "n"; Lconst (Cint 1)]),
      Lconst (Cint 1),
      Lapply(Lvar "mul", [Lvar "n"; Lapply(Lvar "fac", [Lapply(Lvar "sub", [Lvar "n"; Lconst (Cint 1)])])])))]),
  Llet("result", Lapply(Lvar "fac", [Lconst (Cint 5)]), Lvar "result"))
```

### Generated JVM Bytecode (pseudocode)
```
.class Factorial
.method public static fac(I)I
  aload_0                 ; load n
  iconst_1                ; load 1
  if_icmple THEN          ; if n <= 1, jump to THEN
  aload_0                 ; else: load n
  aload_0                 ; load n
  iconst_1                ; load 1
  isub                    ; n - 1
  invokestatic Factorial.fac(I)I  ; recursive call
  imul                    ; multiply
  ireturn
THEN:
  iconst_1                ; return 1
  ireturn
.end method
```

---

## 12. Testing Strategy

```ocaml
(* test/test_typing.ml *)
let test_simple_inference () =
  let expr = Fun ("x", BinOp (Add, Var "x", Int 1)) in
  match Typing.infer [] expr with
  | Some (TyFun (TyInt, TyInt), _) -> Printf.printf "✓ Function typing\n"
  | _ -> failwith "Type inference failed"

(* test/test_codegen.ml *)
let test_int_arithmetic () =
  let lam = Llet ("x", Lconst (Cint 5), 
                  Llet ("y", Lconst (Cint 3),
                        Lapply (Lvar "add", [Lvar "x"; Lvar "y"]))) in
  let state = { code = []; stack_depth = 0; local_vars = []; next_label = 0 } in
  JvmGen.emit_lambda state lam;
  assert (state.stack_depth >= 0);
  Printf.printf "✓ Codegen produces valid stack\n"
```

---

## 13. Performance Considerations

| Aspect | Strategy |
|--------|----------|
| **GC** | Rely on JVM GC; use generational heap |
| **Closure Allocation** | Cache frequently used closures |
| **Pattern Matching** | Compile to jump tables for large matches |
| **Specialization** | Monomorphize generic functions for hot paths |
| **Inlining** | Inline hot functions at codegen time |
| **Tail Calls** | Use CPS to identify; JVM `goto` for TCO |

---

## 14. Integration with Java Ecosystem

### FFI Example

```ocaml
(* Define external Java binding *)
external java_string_length : string -> int = "StringLength"

(* Implementation bridges to Java via JNI *)
(* Bytecode: INVOKESTATIC java/lang/String.length()I *)
```

### Seamless Interop
- Call Java methods from OCaml: `let x = java_method arg`
- Return OCaml values to Java: automatically boxed
- Use Java collections: `List` → `java.util.ArrayList`

---

## 15. Milestones & Roadmap

| Phase | Task | Duration |
|-------|------|----------|
| **Phase 1** | Lexer, parser, AST | 2-3 weeks |
| **Phase 2** | Type inference engine | 3-4 weeks |
| **Phase 3** | Lambda IR + optimization | 2-3 weeks |
| **Phase 4** | Basic JVM codegen (integers, functions) | 3-4 weeks |
| **Phase 5** | Advanced types (lists, records, variants) | 3-4 weeks |
| **Phase 6** | Tail-call optimization, CPS | 2-3 weeks |
| **Phase 7** | Standard library + tests | 2-3 weeks |
| **Phase 8** | Production hardening | 2-3 weeks |

**Total: ~20-25 weeks for a feature-complete v1**

---

## 16. Reference Implementation Patterns

### Pattern 1: Algebraic Data Types in OCaml (for Compiler)
```ocaml
type expr = 
  | Int of int
  | Fun of string * expr
  | App of expr * expr

(* Pattern matching handles all cases; compiler checks exhaustiveness *)
let rec eval = function
  | Int i -> i
  | Fun (x, body) -> (* closure *)
  | App (f, arg) -> eval f (eval arg)
```

### Pattern 2: Two-Level Language
```ocaml
(* Level 1: High-level AST (what user writes) *)
type expr = Fun of string * expr | App of expr * expr

(* Level 2: Low-level Lambda (what JVM executes) *)
type lambda = Lfunction of string list * lambda | Lapply of lambda * lambda list

(* Compiler: lower level 1 to level 2 *)
let rec lower : expr -> lambda = ...
```

### Pattern 3: Environment-Based Semantics
```ocaml
type env = (string * value) list

let rec eval (env : env) (e : expr) : value =
  match e with
  | Var x -> List.assoc x env
  | Fun (x, body) -> VClosure (x, body, env)  (* capture env *)
  | App (f, arg) -> 
      let VClosure (x, body, env') = eval env f in
      eval ((x, eval env arg) :: env') body
```

---

## 17. Known Challenges & Solutions

| Challenge | Solution |
|-----------|----------|
| **Stack-based JVM vs. ML closures** | Compile closures to Java objects implementing `Callable` |
| **Tail recursion without `goto`** | Use CPS conversion; JVM will inline calls |
| **Type erasure** | Monomorphize hot paths; use runtime type tags for generics |
| **Pattern match dispatch** | Compile to decision trees; JVM optimizes to jump tables |
| **Immutability in JVM** | Use persistent data structures (HAMT for maps/sets) |

---

## 18. Conclusion

This architecture combines:
- **OCaml's strengths**: algebraic types, pattern matching, strong type system, functional idioms
- **JVM's strengths**: performance, mature ecosystem, bytecode stability, cross-platform
- **Multi-stage compilation**: clean separation between parsing, typing, IR, codegen

The result is a robust, extensible compiler for a functional language targeting a production-grade runtime.
