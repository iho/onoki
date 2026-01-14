(* JVM Class File Format Writer *)

(* Helper functions for writing binary data *)
let write_u1 (buf : Buffer.t) (n : int) : unit =
  Buffer.add_char buf (char_of_int (n land 0xFF))

let write_u2 (buf : Buffer.t) (n : int) : unit =
  Buffer.add_char buf (char_of_int ((n lsr 8) land 0xFF));
  Buffer.add_char buf (char_of_int (n land 0xFF))

let write_u4 (buf : Buffer.t) (n : int32) : unit =
  let n = Int32.to_int n in
  Buffer.add_char buf (char_of_int ((n lsr 24) land 0xFF));
  Buffer.add_char buf (char_of_int ((n lsr 16) land 0xFF));
  Buffer.add_char buf (char_of_int ((n lsr 8) land 0xFF));
  Buffer.add_char buf (char_of_int (n land 0xFF))

(* Constant pool entries *)
type cp_entry =
  | CP_Utf8 of string
  | CP_Integer of int32
  | CP_Float of float
  | CP_Class of int  (* name_index *)
  | CP_String of int  (* string_index *)
  | CP_Fieldref of int * int  (* class_index, name_and_type_index *)
  | CP_Methodref of int * int
  | CP_InterfaceMethodref of int * int
  | CP_NameAndType of int * int  (* name_index, descriptor_index *)

(* Constant pool builder *)
type cp_builder = {
  mutable entries: cp_entry list;
  mutable index_map: (cp_entry * int) list;
}

let create_cp_builder () : cp_builder =
  { entries = []; index_map = [] }

let add_cp_entry (cpb : cp_builder) (entry : cp_entry) : int =
  (* Check if entry already exists *)
  match List.assoc_opt entry cpb.index_map with
  | Some idx -> idx
  | None ->
      cpb.entries <- cpb.entries @ [entry];
      let idx = List.length cpb.entries in
      cpb.index_map <- (entry, idx) :: cpb.index_map;
      idx

let add_utf8 (cpb : cp_builder) (s : string) : int =
  add_cp_entry cpb (CP_Utf8 s)

let add_class (cpb : cp_builder) (name : string) : int =
  let name_idx = add_utf8 cpb name in
  add_cp_entry cpb (CP_Class name_idx)

let add_string (cpb : cp_builder) (s : string) : int =
  let utf8_idx = add_utf8 cpb s in
  add_cp_entry cpb (CP_String utf8_idx)

let add_integer (cpb : cp_builder) (n : int32) : int =
  add_cp_entry cpb (CP_Integer n)

let add_name_and_type (cpb : cp_builder) (name : string) (descriptor : string) : int =
  let name_idx = add_utf8 cpb name in
  let desc_idx = add_utf8 cpb descriptor in
  add_cp_entry cpb (CP_NameAndType (name_idx, desc_idx))

let add_methodref (cpb : cp_builder) (class_name : string) (method_name : string) (descriptor : string) : int =
  let class_idx = add_class cpb class_name in
  let nat_idx = add_name_and_type cpb method_name descriptor in
  add_cp_entry cpb (CP_Methodref (class_idx, nat_idx))

let add_fieldref (cpb : cp_builder) (class_name : string) (field_name : string) (descriptor : string) : int =
  let class_idx = add_class cpb class_name in
  let nat_idx = add_name_and_type cpb field_name descriptor in
  add_cp_entry cpb (CP_Fieldref (class_idx, nat_idx))

let add_interface_methodref (cpb : cp_builder) (class_name : string) (method_name : string) (descriptor : string) : int =
  let class_idx = add_class cpb class_name in
  let nat_idx = add_name_and_type cpb method_name descriptor in
  add_cp_entry cpb (CP_InterfaceMethodref (class_idx, nat_idx))

(* Write constant pool entry *)
let write_cp_entry (buf : Buffer.t) (entry : cp_entry) : unit =
  match entry with
  | CP_Utf8 s ->
      write_u1 buf 1;  (* tag *)
      write_u2 buf (String.length s);
      Buffer.add_string buf s
  
  | CP_Integer n ->
      write_u1 buf 3;  (* tag *)
      write_u4 buf n
  
  | CP_Float f ->
      write_u1 buf 4;  (* tag *)
      write_u4 buf (Int32.bits_of_float f)
  
  | CP_Class name_idx ->
      write_u1 buf 7;  (* tag *)
      write_u2 buf name_idx
  
  | CP_String string_idx ->
      write_u1 buf 8;  (* tag *)
      write_u2 buf string_idx
  
  | CP_Fieldref (class_idx, nat_idx) ->
      write_u1 buf 9;  (* tag *)
      write_u2 buf class_idx;
      write_u2 buf nat_idx
  
  | CP_Methodref (class_idx, nat_idx) ->
      write_u1 buf 10;  (* tag *)
      write_u2 buf class_idx;
      write_u2 buf nat_idx
  
  | CP_InterfaceMethodref (class_idx, nat_idx) ->
      write_u1 buf 11;  (* tag *)
      write_u2 buf class_idx;
      write_u2 buf nat_idx
  
  | CP_NameAndType (name_idx, desc_idx) ->
      write_u1 buf 12;  (* tag *)
      write_u2 buf name_idx;
      write_u2 buf desc_idx

(* Write constant pool *)
let write_constant_pool (buf : Buffer.t) (cpb : cp_builder) : unit =
  let count = List.length cpb.entries + 1 in  (* +1 because index 0 is reserved *)
  write_u2 buf count;
  List.iter (write_cp_entry buf) cpb.entries

(* Attributes *)
type attribute =
  | Code of {
      max_stack: int;
      max_locals: int;
      code: bytes;
      exception_table: exception_handler list;
      attributes: attribute list;
    }
  | SourceFile of int  (* sourcefile_index *)

and exception_handler = {
  start_pc: int;
  end_pc: int;
  handler_pc: int;
  catch_type: int;
}

(* Write attribute *)
let rec write_attribute (buf : Buffer.t) (cpb : cp_builder) (attr : attribute) (code_name_idx : int) : unit =
  match attr with
  | Code { max_stack; max_locals; code; exception_table; attributes } ->
      write_u2 buf code_name_idx;
      
      (* Calculate attribute length *)
      let attr_len = 
        2 + 2 + 4 + Bytes.length code + 
        2 + (List.length exception_table * 8) +
        2 (* attributes count, empty for now *)
      in
      write_u4 buf (Int32.of_int attr_len);
      
      write_u2 buf max_stack;
      write_u2 buf max_locals;
      write_u4 buf (Int32.of_int (Bytes.length code));
      Buffer.add_bytes buf code;
      
      (* Exception table *)
      write_u2 buf (List.length exception_table);
      List.iter (fun eh ->
        write_u2 buf eh.start_pc;
        write_u2 buf eh.end_pc;
        write_u2 buf eh.handler_pc;
        write_u2 buf eh.catch_type
      ) exception_table;
      
      (* Nested attributes *)
      write_u2 buf (List.length attributes);
      List.iter (fun a -> write_attribute buf cpb a code_name_idx) attributes
  
  | SourceFile sourcefile_idx ->
      let name_idx = List.assoc (CP_Utf8 "SourceFile") cpb.index_map in
      write_u2 buf name_idx;
      write_u4 buf 2l;  (* attribute length *)
      write_u2 buf sourcefile_idx

(* Method info *)
type method_info = {
  access_flags: int;
  name_index: int;
  descriptor_index: int;
  attributes: attribute list;
}

let write_method (buf : Buffer.t) (cpb : cp_builder) (code_name_idx : int) (m : method_info) : unit =
  write_u2 buf m.access_flags;
  write_u2 buf m.name_index;
  write_u2 buf m.descriptor_index;
  write_u2 buf (List.length m.attributes);
  List.iter (fun a -> write_attribute buf cpb a code_name_idx) m.attributes

(* Field info *)
type field_info = {
  access_flags: int;
  name_index: int;
  descriptor_index: int;
  attributes: attribute list;
}

let write_field (buf : Buffer.t) (cpb : cp_builder) (code_name_idx : int) (f : field_info) : unit =
  write_u2 buf f.access_flags;
  write_u2 buf f.name_index;
  write_u2 buf f.descriptor_index;
  write_u2 buf (List.length f.attributes);
  List.iter (fun a -> write_attribute buf cpb a code_name_idx) f.attributes

(* Class file *)
type class_file = {
  minor_version: int;
  major_version: int;
  constant_pool: cp_builder;
  access_flags: int;
  this_class: int;
  super_class: int;
  interfaces: int list;
  fields: field_info list;
  methods: method_info list;
  attributes: attribute list;
}

(* Write complete class file *)
let write_class_file (cf : class_file) : bytes =
  let buf = Buffer.create 4096 in
  
  (* Pre-add "Code" attribute name to constant pool *)
  let code_name_idx = add_utf8 cf.constant_pool "Code" in
  
  (* Magic number: 0xCAFEBABE *)
  write_u4 buf 0xCAFEBABEl;
  
  (* Version *)
  write_u2 buf cf.minor_version;
  write_u2 buf cf.major_version;
  
  (* Constant pool *)
  write_constant_pool buf cf.constant_pool;
  
  (* Access flags *)
  write_u2 buf cf.access_flags;
  
  (* This class, super class *)
  write_u2 buf cf.this_class;
  write_u2 buf cf.super_class;
  
  (* Interfaces *)
  write_u2 buf (List.length cf.interfaces);
  List.iter (write_u2 buf) cf.interfaces;
  
  (* Fields *)
  write_u2 buf (List.length cf.fields);
  List.iter (write_field buf cf.constant_pool code_name_idx) cf.fields;
  
  (* Methods *)
  write_u2 buf (List.length cf.methods);
  List.iter (write_method buf cf.constant_pool code_name_idx) cf.methods;
  
  (* Attributes *)
  write_u2 buf (List.length cf.attributes);
  List.iter (fun a -> write_attribute buf cf.constant_pool a code_name_idx) cf.attributes;
  
  Buffer.to_bytes buf

(* Access flags *)
let acc_public = 0x0001
let acc_private = 0x0002
let acc_protected = 0x0004
let acc_static = 0x0008
let acc_final = 0x0010
let acc_super = 0x0020
let acc_synchronized = 0x0020
let acc_volatile = 0x0040
let acc_bridge = 0x0040
let acc_transient = 0x0080
let acc_varargs = 0x0080
let acc_native = 0x0100
let acc_interface = 0x0200
let acc_abstract = 0x0400
let acc_strict = 0x0800
let acc_synthetic = 0x1000
let acc_annotation = 0x2000
let acc_enum = 0x4000
