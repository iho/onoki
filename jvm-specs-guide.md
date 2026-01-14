# JVM Bytecode Specification Reference

## Where to Find Official JVM Specifications

### Official Oracle Documentation (Most Authoritative)

#### 1. **The Java® Virtual Machine Specification - Current Version (Java SE 21)**
- **Full Online Documentation:** https://docs.oracle.com/javase/specs/jvms/se21/html/
- **Table of Contents Includes:**
  - Chapter 2: Structure of the JVM (data types, runtime areas, instruction set overview)
  - Chapter 3: Compiling for the JVM (how to generate bytecode)
  - Chapter 4: Class File Format (detailed binary format specification)
  - Chapter 5: Loading, Linking, Initialization
  - Chapter 6: Java Virtual Machine Instruction Set (complete opcode reference)
  - Chapter 7: Opcode Mnemonics by Opcode (numerical index)

#### 2. **Java SE 8 Edition (Recommended for Learning)**
- **PDF Download:** https://docs.oracle.com/javase/specs/jvms/se8/jvms8.pdf
- **HTML Version:** https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-1.html
- **Why SE 8?** Stable, widely implemented, good foundation for understanding JVM architecture
- **Key Additions in SE 8:**
  - `invokedynamic` instruction (JSR 292)
  - Lambda support
  - Default methods in interfaces
  - Method parameter names at runtime

#### 3. **Java SE 7 Edition (Alternative Reference)**
- **PDF:** https://docs.oracle.com/javase/specs/jvms/se7/html/
- **Use Case:** Lightweight, pre-invokedynamic (simpler if you don't need dynamic language support)

#### 4. **Java SE 11 Edition**
- **HTML:** https://docs.oracle.com/javase/specs/jvms/se11/html/
- **Includes:** Module system, records (SE 16+)

---

## Quick Navigation: Key Specification Chapters

### **Chapter 4: The Class File Format** (Most Important for Code Generation)

Direct links to subsections:

| Section | Topic | Purpose |
|---------|-------|---------|
| 4.1 | ClassFile Structure | Binary layout of `.class` files |
| 4.2-4.3 | Names & Descriptors | How to encode type info (`I` = int, `Z` = boolean, etc.) |
| 4.4 | Constant Pool | Symbol table (CONSTANT_Utf8, CONSTANT_Class, etc.) |
| 4.5 | Fields | Field definition structure |
| 4.6 | Methods | Method definition structure |
| 4.7 | Attributes | Code, StackMapTable, LineNumberTable, etc. |
| 4.9 | Constraints | Static/structural rules for valid bytecode |
| 4.10 | Verification | Bytecode verification rules |

**SE 8 Direct Link:** https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html

### **Chapter 6: Java Virtual Machine Instruction Set** (Bytecode Reference)

**All 200+ opcodes listed alphabetically with:**
- Opcode number (decimal & hex)
- Format diagram
- Operand stack effects
- Linking/runtime exceptions
- Example: `iadd` (0x60) adds two ints

**SE 8 Direct Link:** https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-6.html

### **Chapter 7: Opcode Mnemonics by Opcode** (Lookup Table)

Reverse index: opcode number → mnemonic (fastest way to identify instructions)

**SE 8 Direct Link:** https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-7.html

---

## Type Descriptors Quick Reference

For encoding types in constant pool and method signatures:

```
Primitive Types:
  B  = byte
  C  = char
  D  = double
  F  = float
  I  = int
  J  = long (mnemonic: 'J' because 'L' is reserved for classes)
  S  = short
  Z  = boolean ('Z' for boolean - zero/one)
  V  = void

Reference Types:
  Lclass_name;  = class reference (e.g., Ljava/lang/String;)
  [type         = array (e.g., [I = int[], [[I = int[][])

Method Signatures:
  (arg_types)return_type
  Example: (II)I  = two ints → one int (e.g., int add(int a, int b))
  Example: (Ljava/lang/String;I)Ljava/lang/String;  = String + int → String
```

---

## Bytecode Instruction Categories

### Load/Store Instructions
```
iload, iload_0-3         Load int from local variable
istore, istore_0-3       Store int to local variable
ldc, ldc_w, ldc2_w       Load constant from constant pool
bipush, sipush           Push byte/short to stack
aload, aload_0-3         Load object reference
astore, astore_0-3       Store object reference
```

### Arithmetic Instructions
```
iadd, isub, imul, idiv, irem   int arithmetic (a b → result)
dadd, dsub, dmul, ddiv, drem   double arithmetic
fadd, fsub, fmul, fdiv, frem   float arithmetic
ladd, lsub, lmul, ldiv, lrem   long arithmetic
ineg, dneg, fneg, lneg         Negation
```

### Type Conversion
```
i2l, i2f, i2d     int → long/float/double
l2i, l2f, l2d     long → int/float/double
f2i, f2l, f2d     float → int/long/double
d2i, d2l, d2f     double → int/long/float
i2b, i2c, i2s     int → byte/char/short
```

### Object/Array Instructions
```
new                Create new object instance
newarray           Create primitive array
anewarray          Create object array
multianewarray     Create multidimensional array
getfield           Read instance field
putfield           Write instance field
getstatic          Read static field
putstatic          Write static field
aastore, aaload    Array element operations
```

### Method Invocation
```
invokevirtual      Call instance method (virtual dispatch)
invokestatic       Call static method
invokespecial      Call constructor or private method
invokeinterface    Call interface method
invokedynamic      Dynamic method invocation (Java 7+)
```

### Control Flow
```
ifeq, ifne, iflt, ifle, ifgt, ifge   Branch if int condition
if_icmpeq, if_icmpne, ...            Compare two ints
goto, goto_w                          Unconditional branch
tableswitch                           Jump table (dense cases)
lookupswitch                          Sparse case dispatch
```

### Stack Manipulation
```
dup, dup_x1, dup_x2      Duplicate top value(s)
dup2, dup2_x1, dup2_x2   Duplicate two top values
pop, pop2                Remove from stack
swap                     Swap top two values
```

### Exception Handling
```
athrow              Throw exception
monitorenter        Lock object (synchronized entry)
monitorexit         Unlock object (synchronized exit)
```

### Return Instructions
```
ireturn     Return int value
lreturn     Return long value
freturn     Return float value
dreturn     Return double value
areturn     Return object reference
return      Return void
```

---

## Class File Binary Format (At a Glance)

```
ClassFile {
    u4             magic;              // 0xCAFEBABE
    u2             minor_version;      // e.g., 0
    u2             major_version;      // e.g., 52 (Java 8), 61 (Java 17)
    u2             constant_pool_count;
    cp_info        constant_pool[constant_pool_count-1];
    u2             access_flags;       // PUBLIC, FINAL, INTERFACE, etc.
    u2             this_class;         // index to CONSTANT_Class
    u2             super_class;        // index to CONSTANT_Class
    u2             interfaces_count;
    u2             interfaces[interfaces_count];
    u2             fields_count;
    field_info     fields[fields_count];
    u2             methods_count;
    method_info    methods[methods_count];
    u2             attributes_count;
    attribute_info attributes[attributes_count];
}

Code Attribute (inside method_info):
{
    u2             attribute_name_index;  // "Code"
    u4             attribute_length;
    u2             max_stack;              // operand stack size
    u2             max_locals;             // local variable array size
    u4             code_length;
    u1             code[code_length];      // THE BYTECODE!
    u2             exception_table_length;
    exception_handler exception_table[...];
    u2             attributes_count;      // e.g., LineNumberTable, LocalVariableTable
    attribute_info attributes[attributes_count];
}
```

---

## Important Versions & Major Version Numbers

```
Java 8 (SE 8)       = Major version 52
Java 9              = Major version 53
Java 10             = Major version 54
Java 11 (LTS)       = Major version 55
Java 12             = Major version 56
Java 13             = Major version 57
Java 14             = Major version 58
Java 15             = Major version 59
Java 16             = Major version 60
Java 17 (LTS)       = Major version 61
Java 18             = Major version 62
Java 19             = Major version 63
Java 20             = Major version 64
Java 21 (LTS)       = Major version 65
```

**Note:** Old Java versions (1.0-1.7) used major versions 45-51. Most compilers target 52+ for modern language features.

---

## Quick Reference for Your OCaml Compiler

### When Generating Bytecode:
1. **Constant Pool Encoding** → Use Chapter 4, Section 4.4
2. **Method Signature** → Use Chapter 4, Section 4.3.3
3. **Instruction Encoding** → Use Chapter 6 (alphabetical) or Chapter 7 (by opcode)
4. **Stack Depths/Types** → Use Chapter 4, Section 4.10 (verification types)

### Minimal Bytecode Example
```
// Compile: int add(int a, int b) { return a + b; }

// Bytecode:
  0: iload_1        // Load parameter a (local[1])
  1: iload_2        // Load parameter b (local[2])
  2: iadd           // Add: pop b, pop a, push result
  3: ireturn        // Return result
```

### Class File Hex Dump Example
```
CA FE BA BE          // magic = 0xCAFEBABE
00 00               // minor_version = 0
00 34               // major_version = 52 (Java 8)
00 07               // constant_pool_count = 7 (1-indexed, so 6 entries)
... (constant pool)
00 01               // access_flags = ACC_PUBLIC
00 04               // this_class = constant #4
00 05               // super_class = constant #5
... (interfaces, fields, methods, attributes)
```

---

## Tools & Alternatives

### Validation & Reference Tools
- **javap** (Java disassembler, comes with JDK)
  ```bash
  javap -c -v MyClass.class  # Shows all bytecode + constant pool
  ```

- **CFR** (Modern Java decompiler)
  https://www.benf.org/other/cfr/

- **Jasmin** (Bytecode assembler - write bytecode in text form)
  https://jasmin.sourceforge.net/

- **ASM** (Java bytecode engineering library)
  https://asm.ow2.io/ (Great for understanding bytecode generation)

### Online Opcode Database
- https://en.wikipedia.org/wiki/List_of_JVM_bytecode_instructions (Quick reference)
- https://www.cs.miami.edu/home/burt/reference/java/ (Alternative spec mirror)

---

## Stability Guarantee

**Why bytecode format is stable:**

The JVM spec is **maintained by Oracle under JSR (Java Specification Request) process**. Changes require:
1. Expert group consensus
2. Public review
3. Compatibility requirements

**New opcodes or features** (like `invokedynamic` in Java 7, records in Java 16) are:
- **Backward compatible** - old bytecode still runs
- **Evolutionary** - minor version can increment
- **Rarely breaking** - only at major version boundaries (e.g., Java 8 → 9)

This means: **Write your compiler to Java 8 bytecode format, and it will likely run on Java 21+ without changes.**

---

## Summary for Your OCaml JVM Compiler

| Need | Resource |
|------|----------|
| **Class file format** | https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html |
| **All bytecode instructions** | https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-6.html |
| **Quick opcode lookup** | https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-7.html |
| **Type descriptors** | https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.3.html |
| **Verification rules** | https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.10.html |
| **Constant pool format** | https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.4.html |
| **Code attribute** | https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.7.3.html |

**Recommendation:** Download the SE 8 PDF (stable, comprehensive, ~500 pages) and keep it in your project directory as you implement.
