(* JVM instructions *)
type instruction =
  (* Constants *)
  | Iconst_m1
  | Iconst_0 | Iconst_1 | Iconst_2 | Iconst_3 | Iconst_4 | Iconst_5
  | Bipush of int  (* -128 to 127 *)
  | Sipush of int  (* -32768 to 32767 *)
  | Ldc of int     (* constant pool index *)
  | Ldc_w of int
  
  (* Load/Store local variables *)
  | Iload of int
  | Iload_0 | Iload_1 | Iload_2 | Iload_3
  | Aload of int
  | Aload_0 | Aload_1 | Aload_2 | Aload_3
  | Fload of int
  | Fload_0 | Fload_1 | Fload_2 | Fload_3
  | Dload of int
  | Dload_0 | Dload_1 | Dload_2 | Dload_3
  
  | Istore of int
  | Istore_0 | Istore_1 | Istore_2 | Istore_3
  | Astore of int
  | Astore_0 | Astore_1 | Astore_2 | Astore_3
  | Fstore of int
  | Fstore_0 | Fstore_1 | Fstore_2 | Fstore_3
  | Dstore of int
  | Dstore_0 | Dstore_1 | Dstore_2 | Dstore_3
  
  (* Stack manipulation *)
  | Pop
  | Pop2
  | Dup
  | Dup_x1
  | Dup_x2
  | Dup2
  | Swap
  
  (* Arithmetic *)
  | Iadd | Isub | Imul | Idiv | Irem
  | Ineg
  | Dadd | Dsub | Dmul | Ddiv | Drem
  | Dneg
  
  (* Bitwise *)
  | Ishl | Ishr | Iushr
  | Iand | Ior | Ixor
  
  (* Comparison *)
  | Ifeq of int  (* branch offset *)
  | Ifne of int
  | Iflt of int
  | Ifle of int
  | Ifgt of int
  | Ifge of int
  | If_icmpeq of int
  | If_icmpne of int
  | If_icmplt of int
  | If_icmple of int
  | If_icmpgt of int
  | If_icmpge of int
  | If_acmpeq of int
  | If_acmpne of int
  | Goto of int
  
  (* Method invocation *)
  | Invokevirtual of int  (* methodref index *)
  | Invokespecial of int
  | Invokestatic of int
  | Invokeinterface of int * int  (* methodref index, count *)
  
  (* Object/Array *)
  | New of int  (* class index *)
  | Newarray of int  (* atype *)
  | Anewarray of int  (* class index *)
  | Arraylength
  | Iaload | Aaload
  | Iastore | Aastore
  | Getstatic of int  (* fieldref index *)
  | Putstatic of int
  | Getfield of int
  | Putfield of int
  
  (* Return *)
  | Ireturn
  | Areturn
  | Return
  
  (* Other *)
  | Nop
  | Athrow
  | Checkcast of int (* class index *)
  | Instanceof of int (* class index *)

(* Encode single instruction to bytes *)
let encode_instruction (instr : instruction) : bytes =
  let buf = Buffer.create 8 in
  let add_u1 n = Buffer.add_char buf (char_of_int (n land 0xFF)) in
  let add_u2 n =
    Buffer.add_char buf (char_of_int ((n lsr 8) land 0xFF));
    Buffer.add_char buf (char_of_int (n land 0xFF))
  in
  let add_i2 n =
    (* Signed 16-bit *)
    let n = if n < 0 then n + 65536 else n in
    add_u2 n
  in
  
  (match instr with
   (* Constants *)
   | Iconst_m1 -> add_u1 0x02
   | Iconst_0 -> add_u1 0x03
   | Iconst_1 -> add_u1 0x04
   | Iconst_2 -> add_u1 0x05
   | Iconst_3 -> add_u1 0x06
   | Iconst_4 -> add_u1 0x07
   | Iconst_5 -> add_u1 0x08
   | Bipush n -> add_u1 0x10; add_u1 n
   | Sipush n -> add_u1 0x11; add_i2 n
   | Ldc idx -> add_u1 0x12; add_u1 idx
   | Ldc_w idx -> add_u1 0x13; add_u2 idx
   
   (* Load *)
   | Iload n -> add_u1 0x15; add_u1 n
   | Iload_0 -> add_u1 0x1a
   | Iload_1 -> add_u1 0x1b
   | Iload_2 -> add_u1 0x1c
   | Iload_3 -> add_u1 0x1d
   | Aload n -> add_u1 0x19; add_u1 n
   | Aload_0 -> add_u1 0x2a
   | Aload_1 -> add_u1 0x2b
   | Aload_2 -> add_u1 0x2c
   | Aload_3 -> add_u1 0x2d
   | Fload n -> add_u1 0x17; add_u1 n
   | Fload_0 -> add_u1 0x22
   | Fload_1 -> add_u1 0x23
   | Fload_2 -> add_u1 0x24
   | Fload_3 -> add_u1 0x25
   | Dload n -> add_u1 0x18; add_u1 n
   | Dload_0 -> add_u1 0x26
   | Dload_1 -> add_u1 0x27
   | Dload_2 -> add_u1 0x28
   | Dload_3 -> add_u1 0x29
   
   (* Store *)
   | Istore n -> add_u1 0x36; add_u1 n
   | Istore_0 -> add_u1 0x3b
   | Istore_1 -> add_u1 0x3c
   | Istore_2 -> add_u1 0x3d
   | Istore_3 -> add_u1 0x3e
   | Astore n -> add_u1 0x3a; add_u1 n
   | Astore_0 -> add_u1 0x4b
   | Astore_1 -> add_u1 0x4c
   | Astore_2 -> add_u1 0x4d
   | Astore_3 -> add_u1 0x4e
   | Fstore n -> add_u1 0x38; add_u1 n
   | Fstore_0 -> add_u1 0x43
   | Fstore_1 -> add_u1 0x44
   | Fstore_2 -> add_u1 0x45
   | Fstore_3 -> add_u1 0x46
   | Dstore n -> add_u1 0x39; add_u1 n
   | Dstore_0 -> add_u1 0x47
   | Dstore_1 -> add_u1 0x48
   | Dstore_2 -> add_u1 0x49
   | Dstore_3 -> add_u1 0x4a
   
   (* Stack *)
   | Pop -> add_u1 0x57
   | Pop2 -> add_u1 0x58
   | Dup -> add_u1 0x59
   | Dup_x1 -> add_u1 0x5a
   | Dup_x2 -> add_u1 0x5b
   | Dup2 -> add_u1 0x5c
   | Swap -> add_u1 0x5f
   
   (* Arithmetic *)
   | Iadd -> add_u1 0x60
   | Isub -> add_u1 0x64
   | Imul -> add_u1 0x68
   | Idiv -> add_u1 0x6c
   | Irem -> add_u1 0x70
   | Ineg -> add_u1 0x74
   | Dadd -> add_u1 0x63
   | Dsub -> add_u1 0x67
   | Dmul -> add_u1 0x6b
   | Ddiv -> add_u1 0x6f
   | Drem -> add_u1 0x73
   | Dneg -> add_u1 0x77
   
   (* Bitwise *)
   | Ishl -> add_u1 0x78
   | Ishr -> add_u1 0x7a
   | Iushr -> add_u1 0x7c
   | Iand -> add_u1 0x7e
   | Ior -> add_u1 0x80
   | Ixor -> add_u1 0x82
   
   (* Comparison/Branch *)
   | Ifeq offset -> add_u1 0x99; add_i2 offset
   | Ifne offset -> add_u1 0x9a; add_i2 offset
   | Iflt offset -> add_u1 0x9b; add_i2 offset
   | Ifle offset -> add_u1 0x9c; add_i2 offset
   | Ifgt offset -> add_u1 0x9d; add_i2 offset
   | Ifge offset -> add_u1 0x9e; add_i2 offset
   | If_icmpeq offset -> add_u1 0x9f; add_i2 offset
   | If_icmpne offset -> add_u1 0xa0; add_i2 offset
   | If_icmplt offset -> add_u1 0xa1; add_i2 offset
   | If_icmple offset -> add_u1 0xa4; add_i2 offset
   | If_icmpgt offset -> add_u1 0xa3; add_i2 offset
   | If_icmpge offset -> add_u1 0xa2; add_i2 offset
   | If_acmpeq offset -> add_u1 0xa5; add_i2 offset
   | If_acmpne offset -> add_u1 0xa6; add_i2 offset
   | Goto offset -> add_u1 0xa7; add_i2 offset
   
   (* Method invocation *)
   | Invokevirtual idx -> add_u1 0xb6; add_u2 idx
   | Invokespecial idx -> add_u1 0xb7; add_u2 idx
   | Invokestatic idx -> add_u1 0xb8; add_u2 idx
   | Invokeinterface (idx, count) -> add_u1 0xb9; add_u2 idx; add_u1 count; add_u1 0
   
   (* Object/Array *)
   | New idx -> add_u1 0xbb; add_u2 idx
   | Newarray atype -> add_u1 0xbc; add_u1 atype
   | Anewarray idx -> add_u1 0xbd; add_u2 idx
   | Arraylength -> add_u1 0xbe
   | Iaload -> add_u1 0x2e
   | Aaload -> add_u1 0x32
   | Iastore -> add_u1 0x4f
   | Aastore -> add_u1 0x53
   | Getstatic idx -> add_u1 0xb2; add_u2 idx
   | Putstatic idx -> add_u1 0xb3; add_u2 idx
   | Getfield idx -> add_u1 0xb4; add_u2 idx
   | Putfield idx -> add_u1 0xb5; add_u2 idx
   
   (* Return *)
   | Ireturn -> add_u1 0xac
   | Areturn -> add_u1 0xb0
   | Return -> add_u1 0xb1
   
   (* Other *)
   | Nop -> add_u1 0x00
   | Athrow -> add_u1 0xbf
   | Checkcast idx -> add_u1 0xc0; add_u2 idx
  | Instanceof idx -> add_u1 0xc1; add_u2 idx
  );
  
  Buffer.to_bytes buf

(* Encode list of instructions *)
let encode_instructions (instrs : instruction list) : bytes =
  let buf = Buffer.create 256 in
  List.iter (fun instr ->
    Buffer.add_bytes buf (encode_instruction instr)
  ) instrs;
  Buffer.to_bytes buf

(* Helper: get instruction size in bytes *)
let instruction_size (instr : instruction) : int =
  Bytes.length (encode_instruction instr)

(* Calculate bytecode offsets for labels *)
type label = int

let label_counter = ref 0

let fresh_label () : label =
  incr label_counter;
  !label_counter

let reset_labels () : unit =
  label_counter := 0
