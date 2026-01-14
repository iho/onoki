# Onoki

A functional OCaml-like language targeting the JVM, written in OCaml.

## Overview

Onoki is a compiler for a functional ML-like language that generates JVM bytecode. The project demonstrates:

- **Multi-stage compilation pipeline**: Source → Lexer → Parser → AST → Type Checker → Lambda IR → JVM Bytecode
- **OCaml's strengths**: Algebraic data types, pattern matching, and strong module system for clean compiler implementation
- **JVM as target**: Leveraging the mature JVM ecosystem and cross-platform execution

## Architecture

The compiler follows a traditional multi-stage design:

1. **Lexer** (`src/lexer.mll`) - Tokenization using ocamllex
2. **Parser** (`src/parser.mly`) - Parsing to AST using Menhir
3. **AST** (`src/ast.ml`) - Abstract syntax tree definitions
4. **Type Checker** (`src/typing.ml`) - Hindley-Milner type inference
5. **Lambda IR** (`src/lambda.ml`) - Intermediate representation
6. **Code Generator** (TODO) - JVM bytecode generation

## Building

```bash
# Install dependencies
opam install . --deps-only

# Build
dune build

# Run compiler
dune exec onoki tests/simple.onoki
```

## Language Features

### Currently Supported

- Primitive types: `int`, `float`, `string`, `bool`
- Arithmetic and logical operators
- Functions and lambda expressions
- Let bindings (recursive and non-recursive)
- If-then-else expressions
- Pattern matching
- Lists and tuples
- Records

### Example

```ocaml
let factorial n =
  if n <= 1 then 
    1 
  else 
    n * factorial (n - 1)

let result = factorial 5
```

## Project Status

This is an early-stage implementation. Currently implemented:

- ✅ Lexer and parser
- ✅ AST definitions
- ✅ Basic type inference
- ✅ Lambda IR lowering
- 🚧 JVM bytecode generation (TODO)
- 🚧 Standard library (TODO)
- 🚧 Optimization passes (TODO)

## Documentation

- [`ocaml_jvm_arch.md`](ocaml_jvm_arch.md) - Complete architecture documentation
- [`impl_quickstart.md`](impl_quickstart.md) - Implementation guide
- [`jvm-specs-guide.md`](jvm-specs-guide.md) - JVM bytecode specification reference

## Requirements

- OCaml 5.3 or higher
- Menhir (parser generator)
- Dune (build system)

## License

MIT