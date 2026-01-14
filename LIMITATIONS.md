# Onoki Compiler - Known Limitations

## Language Features

### Type System
- **No polymorphic external signatures**: Cannot use `'a` type variables in `external` declarations
  ```ocaml
  (* Not supported *)
  external add : 'a list -> 'a -> bool = "..."
  
  (* Must use concrete types *)
  external add : int list -> int -> bool = "..."
  ```

- **Incomplete type inference for some constructs**: Type checker may report "Type checking failed" even when code compiles and runs correctly

- **No `unit` type**: Cannot properly call 0-argument Java constructors via externals

### Closures
- **Nested closure captures incomplete**: Closures cannot capture variables from outer closures that are themselves parameters of higher-order functions

### Pattern Matching
- **No guard clauses**: Cannot use `when` conditions in match patterns
  ```ocaml
  (* Not supported *)
  match x with
  | n when n > 0 -> "positive"
  | _ -> "non-positive"
  ```

- **No `as` patterns**: Cannot bind a pattern to a name while matching
  ```ocaml
  (* Not supported *)
  match opt with
  | Some _ as x -> x
  ```

## Java Interop

### External Functions
- **0-argument constructors**: Cannot use `new` externals with empty constructors
  ```ocaml
  (* Problematic: no way to call with 0 args *)
  external new_list : unit -> int list = "new java/util/ArrayList <init> ()V"
  ```

- **No method overloading support**: Must use unique names for overloaded Java methods

- **No exception handling**: Cannot catch Java exceptions

- **No interface implementation**: Cannot implement Java interfaces from Onoki

### Types
- **No Java array support**: Cannot create or manipulate Java arrays directly
- **Limited primitive mapping**: Only `int`, `bool`, `string` mapped to Java types

## Module System
- **No `open` directive**: Must use fully qualified names (`Math.add`) for module members
- **No module signatures/interfaces**: Cannot declare abstract module types
- **No functors**: No parameterized modules

## Syntax
- **No `let ... and ...`**: Cannot define mutually recursive values in one declaration
- **No character literals**: Single quotes `'a'` not supported
- **No multi-line strings**: String literals must be on single line

## Compiler
- **Single file output**: All classes generated in current directory
- **No optimization passes**: No dead code elimination, inlining, etc.
- **Verbose error messages**: Parse/type errors don't always indicate exact location

## Runtime
- **No garbage collection tuning**: Uses default JVM GC
- **No tail call optimization**: Recursive functions can overflow stack
- **Boxed primitives**: All values are boxed Objects, no primitive optimizations
