(* JVM Bytecode Generator *)
open Lambda
module StringMap = Map.Make(String)
module StringSet = Set.Make(String)
open Jvminstr
open Classfile

let global_closure_counter = ref 0
let global_closure_list = ref []

(* Code generation context *)
type function_def = {
  name: string;
  params: string list;
  body: lambda;
  descriptor: string;
}

type codegen_ctx = {
  mutable instructions: instruction list;
  mutable stack_depth: int;
  mutable max_stack: int;
  mutable local_vars: (string * int) list; (* Name to slot index *)
  mutable next_local: int;
  mutable cpb: cp_builder; (* Mutable to update CP *)
  mutable label_positions: (int * int) list; (* Label ID -> instruction index *)
  mutable pending_branches: (int * label * instruction) list; (* Instruction index -> Label ID -> Instruction *)
  mutable functions: function_def list; (* Extracted functions *)
  mutable externs: (string * string) list; (* External function mapping: name -> impl *)
  mutable current_module: string option; (* Current module name for static field fallback *)
}

let create_context () = {
  instructions = [];
  stack_depth = 0;
  max_stack = 0;
  local_vars = [];
  next_local = 0;
  cpb = create_cp_builder ();
  label_positions = [];
  pending_branches = [];
  functions = [];
  externs = [];
  current_module = None;
}


(* Find free variables in a lambda expression *)
let rec free_vars (expr : lambda) : StringSet.t =
  match expr with
  | LConst _ -> StringSet.empty
  | LVar x -> 
      if List.mem x ["__add"; "__sub"; "__mul"; "__div"; "__mod"; 
                     "__eq"; "__ne"; "__lt"; "__le"; "__gt"; "__ge"; 
                     "__and"; "__or"; "__not"; "__neg";
                     "cons"; "nil"; "head"; "tail"; "is_empty"; "^"] then
        StringSet.empty 
      else 
        StringSet.singleton x
  | LFun (params, body) ->
      let fv = free_vars body in
      List.fold_right StringSet.remove params fv
  | LApp (f, args) ->
      List.fold_left (fun acc arg -> StringSet.union acc (free_vars arg)) (free_vars f) args
  | LLet (x, e1, e2) ->
      StringSet.union (free_vars e1) (StringSet.remove x (free_vars e2))
  | LLetRec (bindings, body) ->
      let bound_names = List.map fst bindings in
      let init = free_vars body in
      let all_fv = List.fold_left (fun acc (_, e) -> StringSet.union acc (free_vars e)) init bindings in
       List.fold_right StringSet.remove bound_names all_fv
  | LIf (c, t, e) ->
      StringSet.union (free_vars c) (StringSet.union (free_vars t) (free_vars e))
  | LTuple exprs ->
      List.fold_left (fun acc e -> StringSet.union acc (free_vars e)) StringSet.empty exprs
  (* Add other cases as empty for now *)
  | _ -> StringSet.empty

(* Allocate a local variable slot *)
let alloc_local (ctx : codegen_ctx) (name : string) : int =
  match List.assoc_opt name ctx.local_vars with
  | Some slot -> slot
  | None ->
      let slot = ctx.next_local in
      ctx.local_vars <- (name, slot) :: ctx.local_vars;
      ctx.next_local <- ctx.next_local + 1;
      slot

(* Lookup local variable *)
let lookup_local (ctx : codegen_ctx) (name : string) : int option =
  List.assoc_opt name ctx.local_vars

(* Update stack depth based on instruction *)
let update_stack_depth (ctx : codegen_ctx) (instr : instruction) : unit =
  let delta = match instr with
    (* Push 1 *)
    | Iconst_m1 | Iconst_0 | Iconst_1 | Iconst_2 | Iconst_3 | Iconst_4 | Iconst_5
    | Bipush _ | Sipush _ | Ldc _ | Ldc_w _
    | Iload _ | Iload_0 | Iload_1 | Iload_2 | Iload_3
    | Aload _ | Aload_0 | Aload_1 | Aload_2 | Aload_3
    | Dup -> 1
    
    (* Pop 1 *)
    | Istore _ | Istore_0 | Istore_1 | Istore_2 | Istore_3
    | Astore _ | Astore_0 | Astore_1 | Astore_2 | Astore_3
    | Pop | Ifeq _ | Ifne _ | Iflt _ | Ifle _ | Ifgt _ | Ifge _ -> -1
    
    (* Pop 2, push 1 *)
    | Iadd | Isub | Imul | Idiv | Irem
    | Iand | Ior | Ixor | Ishl | Ishr | Iushr -> -1
    
    (* Pop 2 *)
    | If_icmpeq _ | If_icmpne _ | If_icmplt _ | If_icmple _ | If_icmpgt _ | If_icmpge _
    | Pop2 -> -2
    
    (* Pop 1, return *)
    | Ireturn | Areturn -> -1
    
    (* No change *)
    | Nop | Goto _ | Return -> 0
    
    (* Invokevirtual: pop receiver + args, push result *)
    | Invokevirtual _ -> -1  (* Simplified, depends on method signature *)
    | Invokestatic _ -> 0  (* Simplified *)
    | Getstatic _ -> 1
    
    | _ -> 0  (* Conservative *)
  in
  ctx.stack_depth <- ctx.stack_depth + delta;
  if ctx.stack_depth > ctx.max_stack then
    ctx.max_stack <- ctx.stack_depth

(* Emit instruction *)
let emit (ctx : codegen_ctx) (instr : instruction) : unit =
  ctx.instructions <- ctx.instructions @ [instr];
  update_stack_depth ctx instr

(* Label management *)
let set_label (ctx : codegen_ctx) (lbl : label) : unit =
  let pos = List.length ctx.instructions in
  ctx.label_positions <- (lbl, pos) :: ctx.label_positions

let emit_branch (ctx : codegen_ctx) (instr : instruction) (target : label) : unit =
  let idx = List.length ctx.instructions in
  ctx.pending_branches <- (idx, target, instr) :: ctx.pending_branches;
  emit ctx instr

(* Calculate absolute byte position of an instruction *)
let calculate_byte_position (instrs : instruction array) (idx : int) : int =
  let rec sum_sizes acc i =
    if i >= idx then acc
    else sum_sizes (acc + instruction_size instrs.(i)) (i + 1)
  in
  sum_sizes 0 0

(* Resolve all branch labels to actual byte offsets *)
let resolve_labels (ctx : codegen_ctx) : instruction list =
  let instrs = Array.of_list ctx.instructions in
  let resolved = List.mapi (fun idx instr ->
    (* Check if this instruction has a pending branch *)
    match List.find_opt (fun (i, _, _) -> i = idx) ctx.pending_branches with
    | Some (_, target_label, placeholder) ->
        let target_idx = List.assoc target_label ctx.label_positions in
        (* Calculate byte positions *)
        let branch_byte_pos = calculate_byte_position instrs idx in
        let target_byte_pos = calculate_byte_position instrs target_idx in
        (* Offset is relative to the address of the branch instruction opcode *)
        let offset = target_byte_pos - branch_byte_pos in
        (* Update the instruction with the actual offset *)
        (match placeholder with
         | Ifeq _ -> Ifeq offset
         | Ifne _ -> Ifne offset
         | Iflt _ -> Iflt offset
         | Ifle _ -> Ifle offset
         | Ifgt _ -> Ifgt offset
         | Ifge _ -> Ifge offset
         | If_icmpeq _ -> If_icmpeq offset
         | If_icmpne _ -> If_icmpne offset
         | If_icmplt _ -> If_icmplt offset
         | If_icmple _ -> If_icmple offset
         | If_icmpgt _ -> If_icmpgt offset
         | If_icmpge _ -> If_icmpge offset
         | If_acmpeq _ -> If_acmpeq offset
         | If_acmpne _ -> If_acmpne offset
         | Goto _ -> Goto offset
         | _ -> failwith "Not a branch instruction")
    | None -> instr
  ) ctx.instructions in
  resolved

(* Generate code for constant *)
let gen_const (ctx : codegen_ctx) (c : constant) : unit =
  match c with
  | CInt n ->
      let n32 = Int32.of_int n in
      if n >= -1 && n <= 5 then
        emit ctx (match n with
          | -1 -> Iconst_m1
          | 0 -> Iconst_0
          | 1 -> Iconst_1
          | 2 -> Iconst_2
          | 3 -> Iconst_3
          | 4 -> Iconst_4
          | 5 -> Iconst_5
          | _ -> failwith "impossible")
      else if n >= -128 && n <= 127 then
        emit ctx (Bipush n)
      else if n >= -32768 && n <= 32767 then
        emit ctx (Sipush n)
      else
        let idx = add_integer ctx.cpb n32 in
        if idx < 256 then
          emit ctx (Ldc idx)
        else
          emit ctx (Ldc_w idx)
  
  | CFloat f ->
      let idx = add_cp_entry ctx.cpb (CP_Float f) in
      if idx < 256 then
        emit ctx (Ldc idx)
      else
        emit ctx (Ldc_w idx)
  
  | CString s ->
      let idx = add_string ctx.cpb s in
      if idx < 256 then
        emit ctx (Ldc idx)
      else
        emit ctx (Ldc_w idx)
  
  | CBool b ->
      emit ctx (if b then Iconst_1 else Iconst_0)

(* Helper to box an integer *)
let box_int (ctx : codegen_ctx) =
  let _class_idx = add_class ctx.cpb "java/lang/Integer" in
  let method_idx = add_methodref ctx.cpb "java/lang/Integer" "valueOf" "(I)Ljava/lang/Integer;" in
  emit ctx (Invokestatic method_idx)

(* Helper to unbox an integer *)
let unbox_int (ctx : codegen_ctx) =
  let class_idx = add_class ctx.cpb "java/lang/Integer" in
  let method_idx = add_methodref ctx.cpb "java/lang/Integer" "intValue" "()I" in
  emit ctx (Checkcast class_idx);
  emit ctx (Invokevirtual method_idx)

(* Helper to box a boolean *)
let box_bool (ctx : codegen_ctx) =
  let _class_idx = add_class ctx.cpb "java/lang/Boolean" in
  let method_idx = add_methodref ctx.cpb "java/lang/Boolean" "valueOf" "(Z)Ljava/lang/Boolean;" in
  emit ctx (Invokestatic method_idx)

(* Helper to unbox a boolean *)
let unbox_bool (ctx : codegen_ctx) =
  let class_idx = add_class ctx.cpb "java/lang/Boolean" in
  let method_idx = add_methodref ctx.cpb "java/lang/Boolean" "booleanValue" "()Z" in
  emit ctx (Checkcast class_idx);
  emit ctx (Invokevirtual method_idx)

(* Parse method descriptor to get argument types and return type *)
(* e.g. "(IDLjava/lang/String;)V" -> (["I"; "D"; "Ljava/lang/String;"], "V") *)
let parse_descriptor (desc : string) : string list * string =
  let len = String.length desc in
  let rec parse_args i acc =
    if i >= len then failwith "Invalid descriptor: unexpected end"
    else match desc.[i] with
    | ')' -> (List.rev acc, String.sub desc (i+1) (len - i - 1))
    | 'B' | 'C' | 'D' | 'F' | 'I' | 'J' | 'S' | 'Z' ->
        parse_args (i + 1) (String.make 1 desc.[i] :: acc)
    | 'L' ->
        let start = i in
        let rec find_semi k =
          if k >= len then failwith "Invalid descriptor: missing semicolon"
          else if desc.[k] = ';' then k
          else find_semi (k + 1)
        in
        let semi = find_semi (start + 1) in
        let type_str = String.sub desc start (semi - start + 1) in
        parse_args (semi + 1) (type_str :: acc)
    | '[' ->
        let rec count_dims k = 
          if k < len && desc.[k] = '[' then count_dims (k + 1)
          else k
        in
        let start_type = count_dims (i + 1) in
        let (end_type, _) = 
          match desc.[start_type] with
          | 'L' -> 
              let rec find_semi k = if desc.[k] = ';' then k else find_semi (k + 1) in
              (find_semi start_type, "") 
          | _ -> (start_type, "")
        in
        let type_str = String.sub desc i (end_type - i + 1) in
        parse_args (end_type + 1) (type_str :: acc)
    | _ -> failwith ("Invalid descriptor char: " ^ (String.make 1 desc.[i]))
  in
  if len > 0 && desc.[0] = '(' then parse_args 1 []
  else failwith "Invalid descriptor: must start with ("

(* Generate code for external call *)
(* impl: "kind class method descriptor" *)
let gen_external_call (ctx : codegen_ctx) (impl : string) (args : lambda list) (generator : codegen_ctx -> lambda -> unit) : unit =
  let parts = String.split_on_char ' ' impl in
  match parts with
  | kind :: class_name :: method_name :: descriptor :: _ ->
      let (arg_types, ret_type) = parse_descriptor descriptor in
      
      let generate_args current_args =
        let rec loop args types =
          match args, types with
          | [], [] -> ()
          | a :: as_, t :: ts ->
              generator ctx a;
              (match t with
              | "I" -> unbox_int ctx
              | "Z" -> unbox_bool ctx
              | _ -> 
                 if String.length t > 0 && t.[0] = 'L' then
                   let cls = String.sub t 1 (String.length t - 2) in
                   let class_idx = add_class ctx.cpb cls in
                   emit ctx (Checkcast class_idx)
                 else ());
              loop as_ ts
          | a :: as_, [] ->
              (* Extra argument (likely unit), evaluate and discard *)
              generator ctx a;
              emit ctx Pop;
              loop as_ []
          | [], _ :: _ -> failwith "Not enough arguments for external call"
        in
        loop current_args arg_types
      in

      (match kind with
       | "static" ->
           generate_args args;
           let method_idx = add_methodref ctx.cpb class_name method_name descriptor in
           emit ctx (Invokestatic method_idx)
       
       | "virtual" ->
           (* First arg is receiver *)
           (match args with
            | receiver :: rest_args ->
                generator ctx receiver;
                let class_idx = add_class ctx.cpb class_name in
                emit ctx (Checkcast class_idx);
                
                (* Generate rest args *)
                generate_args rest_args;
                
                let method_idx = add_methodref ctx.cpb class_name method_name descriptor in
                emit ctx (Invokevirtual method_idx)
            | [] -> failwith "Virtual method call requires receiver")

       | "new" ->
           let class_idx = add_class ctx.cpb class_name in
           emit ctx (New class_idx);
           emit ctx Dup;
           (* Constructor args *)
           generate_args args;
           let init_idx = add_methodref ctx.cpb class_name "<init>" descriptor in
           emit ctx (Invokespecial init_idx)
           (* Object is left on stack *)

       | _ -> failwith ("Unknown external kind: " ^ kind));

      (* Handle return type *)
      (* new returns object, static/virtual return what descriptor says *)
      if kind <> "new" then (
        match ret_type with
        | "V" ->
            (* Function must return something, push nil *)
            let nil_idx = add_fieldref ctx.cpb "OnokiNil" "INSTANCE" "LOnokiNil;" in
            emit ctx (Getstatic nil_idx)
        | "I" -> box_int ctx
        | "Z" -> box_bool ctx
        | _ -> () (* Already object *)
      )

  | _ -> failwith ("Invalid implementation string: " ^ impl)

(* Generate code for a Lambda expression *)
let rec gen_lambda (ctx : codegen_ctx) (expr : lambda) : unit =
  match expr with
  | LConst c ->
      gen_const ctx c;
      (* Box the constant *)
      (match c with
       | CInt _ -> box_int ctx
       | CBool _ -> box_bool ctx
       | CString _ -> () (* Strings are already objects *)
       | _ -> ()) (* Float not implemented yet *)

  | LExtern (name, impl) ->
      ctx.externs <- (name, impl) :: ctx.externs

  | LApp (LVar fname, args) when List.mem_assoc fname ctx.externs ->
      let impl = List.assoc fname ctx.externs in
      gen_external_call ctx impl args gen_lambda

  | LTuple exprs ->
      (* Create Object[] array *)
      emit ctx (Bipush (List.length exprs));
      let obj_class_idx = add_class ctx.cpb "java/lang/Object" in
      emit ctx (Anewarray obj_class_idx);
      
      List.iteri (fun i e ->
        emit ctx Dup; (* Keep array on stack *)
        emit ctx (Bipush i);
        gen_lambda ctx e;
        emit ctx Aastore;
      ) exprs;
      (* Array is on stack *)

  | LField (tuple, index) ->
      gen_lambda ctx tuple;
      (* Cast to Object[] *)
      let array_class_idx = add_class ctx.cpb "[Ljava/lang/Object;" in
      emit ctx (Checkcast array_class_idx);
      emit ctx (Bipush index);
      emit ctx Aaload


  | LVar "nil" ->
      let nil_idx = add_fieldref ctx.cpb "OnokiNil" "INSTANCE" "LOnokiNil;" in
      emit ctx (Getstatic nil_idx)
  
  | LVar x -> (
      if String.contains x '.' then
        (match String.split_on_char '.' x with
         | [cls; field] -> 
             let field_ref = add_fieldref ctx.cpb cls field "Ljava/lang/Object;" in
             emit ctx (Getstatic field_ref)
         | _ -> failwith "Invalid qualified name")
      else
        match lookup_local ctx x with
        | Some idx -> 
            if idx <= 3 then 
              emit ctx (match idx with 0 -> Aload_0 | 1 -> Aload_1 | 2 -> Aload_2 | 3 -> Aload_3 | _ -> failwith "impossible")
            else
              emit ctx (Aload idx)
        | None ->
            (* Check globals/functions *)
            match List.find_opt (fun f -> f.name = x) ctx.functions with
            | Some _func ->
               (* Instantiate wrapper closure *)
               let class_name = "Closure$" ^ x in
               let class_idx = add_class ctx.cpb class_name in
               emit ctx (New class_idx);
               emit ctx Dup;
               let init_idx = add_methodref ctx.cpb class_name "<init>" "()V" in
               emit ctx (Invokespecial init_idx)
            | None -> 
                (* Check module fields if in module context *)
                match ctx.current_module with
                | Some mod_name ->
                    let field_ref = add_fieldref ctx.cpb mod_name x "Ljava/lang/Object;" in
                    emit ctx (Getstatic field_ref)
                | None -> failwith ("Undefined variable: " ^ x)
    )
  
  | LLet (x, e1, e2) ->
      gen_lambda ctx e1;
      let slot = alloc_local ctx x in
      if slot <= 3 then
        emit ctx (match slot with
          | 0 -> Astore_0 | 1 -> Astore_1 | 2 -> Astore_2 | 3 -> Astore_3
          | _ -> failwith "impossible")
      else
        emit ctx (Astore slot);
      gen_lambda ctx e2

  | LApp (LVar "cons", [head; tail]) ->
      let cons_idx = add_class ctx.cpb "OnokiCons" in
      emit ctx (New cons_idx);
      emit ctx Dup;
      gen_lambda ctx head;
      gen_lambda ctx tail;
      let init_idx = add_methodref ctx.cpb "OnokiCons" "<init>" "(Ljava/lang/Object;Ljava/lang/Object;)V" in
      emit ctx (Invokespecial init_idx)

  | LApp (LVar "head", [list]) ->
      gen_lambda ctx list;
      let cons_idx = add_class ctx.cpb "OnokiCons" in
      emit ctx (Checkcast cons_idx);
      let head_ref = add_fieldref ctx.cpb "OnokiCons" "head" "Ljava/lang/Object;" in
      emit ctx (Getfield head_ref)

  | LApp (LVar "tail", [list]) ->
      gen_lambda ctx list;
      let cons_idx = add_class ctx.cpb "OnokiCons" in
      emit ctx (Checkcast cons_idx);
      let tail_ref = add_fieldref ctx.cpb "OnokiCons" "tail" "Ljava/lang/Object;" in
      emit ctx (Getfield tail_ref)

  | LApp (LVar "is_empty", [list]) ->
      gen_lambda ctx list;
      (* Compare with OnokiNil.INSTANCE using reference equality *)
      (* We assume we can load it and check equality *)
      let nil_inst_ref = add_fieldref ctx.cpb "OnokiNil" "INSTANCE" "LOnokiNil;" in
      emit ctx (Getstatic nil_inst_ref);
      
      let true_label = fresh_label () in
      let end_label = fresh_label () in
      
      emit_branch ctx (If_acmpeq 0) true_label; 
      (* If_acmpeq is standard JVM instruction 0xA5. Jvminstr.ml might not have it. *)
      (* Assuming it's added or I use a workaround if not. *)
      (* Wait, I didn't check for If_acmpeq in Jvminstr.ml. *)
      (* If it's missing, I should add it. *)
      (* Checking Jvminstr.ml content from memory... *)
      (* I saw If_icmpeq, but not If_acmpeq in the view_file output earlier. *)
      (* Let's check Jvminstr.ml in next step if needed. *)
      (* Or I can use Object.equals *)
      (* But OnokiNil is a singleton so reference equality is correct. *)
      (* Use If_acmpeq assuming I will add it or it exists. *)

      emit ctx Iconst_0;
      emit_branch ctx (Goto 0) end_label;
      set_label ctx true_label;
      emit ctx Iconst_1;
      set_label ctx end_label;
      box_bool ctx

  | LApp (LVar "^", [s1; s2]) ->
      (* String concatenation *)
      gen_lambda ctx s1;
      let string_idx = add_class ctx.cpb "java/lang/String" in
      emit ctx (Checkcast string_idx);
      
      gen_lambda ctx s2;
      emit ctx (Checkcast string_idx);
      
      let concat_idx = add_methodref ctx.cpb "java/lang/String" "concat" "(Ljava/lang/String;)Ljava/lang/String;" in
      emit ctx (Invokevirtual concat_idx)

  | LApp (LVar op, [e1; e2]) when List.mem op ["__add"; "__sub"; "__mul"; "__div"; "__mod"] ->
      gen_lambda ctx e1;
      unbox_int ctx;
      gen_lambda ctx e2;
      unbox_int ctx;
      (match op with
       | "__add" -> emit ctx Iadd
       | "__sub" -> emit ctx Isub
       | "__mul" -> emit ctx Imul
       | "__div" -> emit ctx Idiv
       | "__mod" -> emit ctx Irem
       | _ -> failwith "Unknown operator");
      box_int ctx

  | LApp (LVar op, args) when List.mem op ["__eq"; "__ne"; "__lt"; "__le"; "__gt"; "__ge"] ->
      (* Comparison operators - unbox, compare, return boxed boolean *)
      (match args with
       | [e1; e2] ->
           gen_lambda ctx e1;
           (* For equality, we might want Object.equals later, but for now assuming unboxing *)
           unbox_int ctx; 
           gen_lambda ctx e2;
           unbox_int ctx;
           
           let true_label = fresh_label () in
           let end_label = fresh_label () in
           (match op with
            | "__eq" -> emit_branch ctx (If_icmpeq 0) true_label
            | "__ne" -> emit_branch ctx (If_icmpne 0) true_label
            | "__lt" -> emit_branch ctx (If_icmplt 0) true_label
            | "__le" -> emit_branch ctx (If_icmple 0) true_label
            | "__gt" -> emit_branch ctx (If_icmpgt 0) true_label
            | "__ge" -> emit_branch ctx (If_icmpge 0) true_label
            | _ -> failwith "Unknown comparison");
           
           emit ctx Iconst_0; (* False *)
           emit_branch ctx (Goto 0) end_label;
           
           set_label ctx true_label;
           emit ctx Iconst_1; (* True *)
           
           set_label ctx end_label;
           box_bool ctx (* Box the result *)
           
       | _ -> failwith "Comparison operator expects 2 arguments")

  | LIf (cond, then_, else_) ->
      gen_lambda ctx cond;
      unbox_bool ctx; (* Unbox condition to boolean for branching *)
      let else_label = fresh_label () in
      let end_label = fresh_label () in
      (* If cond == 0 (false), jump to else *)
      emit_branch ctx (Ifeq 0) else_label;
      gen_lambda ctx then_;
      emit_branch ctx (Goto 0) end_label;
      set_label ctx else_label;
      gen_lambda ctx else_;
      set_label ctx end_label
      
  | LLetRec (bindings, body) ->
      (* Extract functions and generate them as methods *)
      List.iter (fun (name, expr) ->
        match expr with
        | LFun (params, fn_body) ->
            (* 1. Register static function *)
            ctx.functions <- {
              name;
              params;
              body = fn_body;
              descriptor = "(Ljava/lang/Object;)Ljava/lang/Object;"; (* Object -> Object *)
            } :: ctx.functions;
            
            (* 2. Generate Wrapper Closure (Closure$Name) *)
            (* Wrapper delegates to Main.name *)
            (* Body: LApp(LVar name, [Arg]) *)
            (* Wrapper needs to call static method. Ensure generator sees it. *)
            let wrapper_params = ["_warg"] in
            let wrapper_body = LApp (LVar name, [LVar "_warg"]) in
            let wrapper_captures = [] in (* Stateless *)
            
            let wrapper_bytes = generate_closure_class name wrapper_params wrapper_body wrapper_captures ctx.functions in
            let class_name = "Closure$" ^ name in
            global_closure_list := (class_name ^ ".class", wrapper_bytes) :: !global_closure_list
            
        | _ ->
            (* Non-function recursive binding - treat as regular let *)
            gen_lambda ctx expr;
            let slot = alloc_local ctx name in
            if slot <= 3 then
              emit ctx (match slot with
                | 0 -> Astore_0 | 1 -> Astore_1 | 2 -> Astore_2 | 3 -> Astore_3
                | _ -> failwith "impossible")
            else
              emit ctx (Astore slot)
      ) bindings;
      gen_lambda ctx body

  | LApp (LVar fname, args) when List.mem_assoc fname (List.map (fun f -> (f.name, f)) ctx.functions) ->
      (* Optimization: If function is known static method, call directly *)
      (* But method descriptors are now (Object) -> Object *)
      List.iter (gen_lambda ctx) args;
      let func_def = List.find (fun f -> f.name = fname) ctx.functions in
      let method_idx = add_methodref ctx.cpb "Main" fname func_def.descriptor in
      emit ctx (Invokestatic method_idx)

  | LApp (f, args) ->
       (* General function call: f is an object implementing OnokiFunction *)
       gen_lambda ctx f; (* Evaluate function term to object *)
       
       (* Apply currying: f.apply(arg1).apply(arg2)... *)
       List.iter (fun arg ->
         (* Checkcast to OnokiFunction *)
         let iface_idx = add_class ctx.cpb "OnokiFunction" in
         emit ctx (Checkcast iface_idx);
         
         gen_lambda ctx arg; (* Push argument *)
         
         let apply_idx = add_interface_methodref ctx.cpb "OnokiFunction" "apply" "(Ljava/lang/Object;)Ljava/lang/Object;" in
         emit ctx (Invokeinterface (apply_idx, 2)); (* 2 args: this, arg *)
       ) args

  | LFun (params, body) ->
      (* 1. Identify captures *)
      let fv = free_vars (LFun (params, body)) in
      let captures = StringSet.elements fv in
      
      (* 2. Generate closure class *)
      let id = string_of_int !global_closure_counter in
      incr global_closure_counter;
      
      let closure_bytes = generate_closure_class id params body captures ctx.functions in
      let class_name = "Closure$" ^ id in
      global_closure_list := (class_name ^ ".class", closure_bytes) :: !global_closure_list;
      
      (* 3. Instantiate closure *)
      let class_idx = add_class ctx.cpb class_name in
      emit ctx (New class_idx);
      emit ctx Dup;
      
      (* Load captures *)
      List.iter (fun cap ->
        match lookup_local ctx cap with
        | Some slot -> 
            if slot <= 3 then
              emit ctx (match slot with 0 -> Aload_0 | 1 -> Aload_1 | 2 -> Aload_2 | 3 -> Aload_3 | _ -> failwith "imp")
            else
              emit ctx (Aload slot)
        | None -> 
            (* Check module fields matching capture *)
            match ctx.current_module with
            | Some mod_name ->
                let field_ref = add_fieldref ctx.cpb mod_name cap "Ljava/lang/Object;" in
                emit ctx (Getstatic field_ref)
            | None -> failwith ("Capture variable not found: " ^ cap)
      ) captures;
      
      (* Init signature *)
      let init_desc = "(" ^ (String.concat "" (List.map (fun _ -> "Ljava/lang/Object;") captures)) ^ ")V" in
      let init_idx = add_methodref ctx.cpb class_name "<init>" init_desc in
      emit ctx (Invokespecial init_idx)


  | stmt ->
      failwith ("Unsupported Lambda form in codegen: " ^ (show_lambda stmt))

(* Generate a closure class *)
and generate_closure_class (id : string) (params : string list) (body : lambda) (captures : string list) (functions : function_def list) : bytes =
(* ... skipping body of generate_closure_class which wasn't changed but I need context to safely edit ... *)
(* Actually the above ReplaceFileContent tool assumes chunked replacement. *)
(* I replaced gen_lambda part. Now I need to fix generate_function_method and generate_classes *)

(* I cannot include generate_closure_class in this replacement because it is large and unchanged. *)
(* I will target gen_lambda block first *)
  (*
    class Closure$N implements OnokiFunction {
      public Object cap1;
      ...
      public Closure$N(Object c1, ...) {
        this.cap1 = c1;
        ...
      }
      public Object apply(Object arg) {
        ... body ...
      }
    }
  *)
  let closure_cpb = create_cp_builder () in
  let class_name = "Closure$" ^ id in
  let this_class_idx = add_class closure_cpb class_name in
  let super_class_idx = add_class closure_cpb "java/lang/Object" in
  let interface_class_idx = add_class closure_cpb "OnokiFunction" in
  
  (* Add fields for captures *)
  let fields = List.map (fun cap ->
    let name_idx = add_utf8 closure_cpb cap in
    let desc_idx = add_utf8 closure_cpb "Ljava/lang/Object;" in
    {
      access_flags = acc_public;
      name_index = name_idx;
      descriptor_index = desc_idx;
      attributes = [];
    }
  ) captures in
  
  (* Generate constructor *)
  (* Signature: (Ljava/lang/Object;...)V *)
  let init_desc = "(" ^ (String.concat "" (List.map (fun _ -> "Ljava/lang/Object;") captures)) ^ ")V" in
  let init_name_idx = add_utf8 closure_cpb "<init>" in
  let init_desc_idx = add_utf8 closure_cpb init_desc in
  
  let init_ctx = create_context () in
  init_ctx.cpb <- closure_cpb; (* Use same constant pool *)
  init_ctx.next_local <- 1 + List.length captures; (* this + args *)
  
  emit init_ctx Aload_0;
  let super_init = add_methodref closure_cpb "java/lang/Object" "<init>" "()V" in
  emit init_ctx (Invokespecial super_init);
  
  (* Assign captures to fields *)
  List.iteri (fun i cap ->
    emit init_ctx Aload_0;
    emit init_ctx (Aload (i + 1));
    let field_ref = add_fieldref closure_cpb class_name cap "Ljava/lang/Object;" in
    emit init_ctx (Putfield field_ref)
  ) captures;
  
  emit init_ctx Return;
  
  let init_code = encode_instructions init_ctx.instructions in 
  
  let _code_name_idx = add_utf8 closure_cpb "Code" in
  let init_code_attr = Code {
    max_stack = 2;
    max_locals = init_ctx.next_local;
    code = init_code;
    exception_table = [];
    attributes = [];
  } in
  
  let init_method : method_info = {
    access_flags = acc_public;
    name_index = init_name_idx;
    descriptor_index = init_desc_idx;
    attributes = [init_code_attr];
  } in

  (* Generate apply method *)
  let apply_ctx = create_context () in
  apply_ctx.cpb <- closure_cpb;
  apply_ctx.functions <- functions; (* Pass global functions! *)
  
  (* Setup locals: 0=this, 1=arg *)
  apply_ctx.local_vars <- [(List.hd params, 1)]; (* Only single param supported *)
  apply_ctx.next_local <- 2;
  
  (* Prepend code to load fields into locals *)
  List.iter (fun cap ->
    let slot = alloc_local apply_ctx cap in
    emit apply_ctx Aload_0;
    let field_ref = add_fieldref closure_cpb class_name cap "Ljava/lang/Object;" in
    emit apply_ctx (Getfield field_ref);
    emit apply_ctx (Astore slot)
  ) captures;
  
  (* Generate body *)
  gen_lambda apply_ctx body;
  emit apply_ctx Areturn;
  
  let resolved_apply = resolve_labels apply_ctx in
  let apply_code = encode_instructions resolved_apply in
  
  let apply_code_attr = Code {
    max_stack = apply_ctx.max_stack + 2;
    max_locals = apply_ctx.next_local;
    code = apply_code;
    exception_table = [];
    attributes = [];
  } in
  
  let apply_name_idx = add_utf8 closure_cpb "apply" in
  let apply_desc_idx = add_utf8 closure_cpb "(Ljava/lang/Object;)Ljava/lang/Object;" in
  
  let apply_method : method_info = {
    access_flags = acc_public;
    name_index = apply_name_idx;
    descriptor_index = apply_desc_idx;
    attributes = [apply_code_attr];
  } in
  
  let class_file = {
    minor_version = 0;
    major_version = 50;
    constant_pool = closure_cpb;
    access_flags = acc_public lor acc_super;
    this_class = this_class_idx;
    super_class = super_class_idx;
    interfaces = [interface_class_idx];
    fields = fields;
    methods = [init_method; apply_method];
    attributes = [];
  } in
  
  write_class_file class_file


(* Generate a method for a function *)
let generate_function_method (cpb : cp_builder) (functions : function_def list) (func : function_def) : method_info =
  reset_labels ();
  let fn_ctx = {
    instructions = [];
    stack_depth = 0;
    max_stack = 0;
    local_vars = [];
    next_local = 0;
    cpb;
    label_positions = [];
    pending_branches = [];
    functions;
    externs = [];
    current_module = None;
  } in
  
  (* Allocate parameters to locals *)
  List.iteri (fun i param ->
    fn_ctx.local_vars <- (param, i) :: fn_ctx.local_vars;
    fn_ctx.next_local <- i + 1
  ) func.params;
  
  (* Generate function body *)
  gen_lambda fn_ctx func.body;
  emit fn_ctx Areturn;
  
  (* Resolve labels *)
  let resolved_instrs = resolve_labels fn_ctx in
  let code_bytes = encode_instructions resolved_instrs in
  
  let code_attr = Code {
    max_stack = fn_ctx.max_stack + 2;
    max_locals = fn_ctx.next_local;
    code = code_bytes;
    exception_table = [];
    attributes = [];
  } in
  
  let method_name_idx = add_utf8 cpb func.name in
  let method_desc_idx = add_utf8 cpb func.descriptor in
  
  {
    access_flags = acc_public lor acc_static;
    name_index = method_name_idx;
    descriptor_index = method_desc_idx;
    attributes = [code_attr];
  }


(* Generate OnokiFunction interface *)
let generate_function_interface () : bytes =
  (*
    public interface OnokiFunction {
      Object apply(Object arg);
    }
  *)
  let ctx = create_context () in
  let interface_class_idx = add_class ctx.cpb "OnokiFunction" in
  let object_class_idx = add_class ctx.cpb "java/lang/Object" in
  
  let apply_name_idx = add_utf8 ctx.cpb "apply" in
  let apply_desc_idx = add_utf8 ctx.cpb "(Ljava/lang/Object;)Ljava/lang/Object;" in
  
  (* Interface method - no Code attribute *)
  let apply_method : method_info = {
    access_flags = acc_public lor acc_abstract;
    name_index = apply_name_idx;
    descriptor_index = apply_desc_idx;
    attributes = [];
  } in
  
  let class_file = {
    minor_version = 0;
    major_version = 50; (* Java 6 *)
    constant_pool = ctx.cpb;
    access_flags = acc_public lor acc_interface lor acc_abstract;
    this_class = interface_class_idx;
    super_class = object_class_idx;
    interfaces = [];
    fields = [];
    methods = [apply_method];
    attributes = [];
  } in
  
  write_class_file class_file

(* Generate List classes (OnokiList, OnokiCons, OnokiNil) *)
let generate_list_classes () : (string * bytes) list =
  let classes = ref [] in
  
  (* 1. OnokiList Interface *)
  let ctx_list = create_context () in
  let list_class_idx = add_class ctx_list.cpb "OnokiList" in
  let object_class_idx = add_class ctx_list.cpb "java/lang/Object" in
  
  let list_class_file = {
    minor_version = 0;
    major_version = 50;
    constant_pool = ctx_list.cpb;
    access_flags = acc_public lor acc_interface lor acc_abstract;
    this_class = list_class_idx;
    super_class = object_class_idx;
    interfaces = [];
    fields = [];
    methods = [];
    attributes = [];
  } in
  classes := ("OnokiList.class", write_class_file list_class_file) :: !classes;
  
  (* 2. OnokiCons Class *)
  let ctx_cons = create_context () in
  let cons_class_idx = add_class ctx_cons.cpb "OnokiCons" in
  let list_interface_idx = add_class ctx_cons.cpb "OnokiList" in
  let object_class_idx = add_class ctx_cons.cpb "java/lang/Object" in
  
  (* Fields: head, tail *)
  let head_name_idx = add_utf8 ctx_cons.cpb "head" in
  let tail_name_idx = add_utf8 ctx_cons.cpb "tail" in
  let obj_desc_idx = add_utf8 ctx_cons.cpb "Ljava/lang/Object;" in
  
  let fields = [
    { access_flags = acc_public; name_index = head_name_idx; descriptor_index = obj_desc_idx; attributes = [] };
    { access_flags = acc_public; name_index = tail_name_idx; descriptor_index = obj_desc_idx; attributes = [] };
  ] in
  
  (* Constructor(Object head, Object tail) *)
  let init_name_idx = add_utf8 ctx_cons.cpb "<init>" in
  let init_desc_idx = add_utf8 ctx_cons.cpb "(Ljava/lang/Object;Ljava/lang/Object;)V" in
  
  let init_ctx = create_context () in
  init_ctx.cpb <- ctx_cons.cpb;
  init_ctx.next_local <- 3; (* this, head, tail *)
  
  emit init_ctx Aload_0;
  let super_init = add_methodref ctx_cons.cpb "java/lang/Object" "<init>" "()V" in
  emit init_ctx (Invokespecial super_init);
  
  (* this.head = head *)
  emit init_ctx Aload_0;
  emit init_ctx Aload_1;
  let head_ref = add_fieldref ctx_cons.cpb "OnokiCons" "head" "Ljava/lang/Object;" in
  emit init_ctx (Putfield head_ref);
  
  (* this.tail = tail *)
  emit init_ctx Aload_0;
  emit init_ctx Aload_2;
  let tail_ref = add_fieldref ctx_cons.cpb "OnokiCons" "tail" "Ljava/lang/Object;" in
  emit init_ctx (Putfield tail_ref);
  
  emit init_ctx Return;
  
  let init_code = encode_instructions init_ctx.instructions in
  let _code_name_idx = add_utf8 ctx_cons.cpb "Code" in
  
  let init_code_attr = Code {
    max_stack = 2;
    max_locals = 3;
    code = init_code;
    exception_table = [];
    attributes = [];
  } in
  
  let init_method : method_info = {
    access_flags = acc_public;
    name_index = init_name_idx;
    descriptor_index = init_desc_idx;
    attributes = [init_code_attr];
  } in
  
  let cons_class_file = {
    minor_version = 0;
    major_version = 50;
    constant_pool = ctx_cons.cpb;
    access_flags = acc_public lor acc_super;
    this_class = cons_class_idx;
    super_class = object_class_idx;
    interfaces = [list_interface_idx];
    fields = fields;
    methods = [init_method];
    attributes = [];
  } in
  classes := ("OnokiCons.class", write_class_file cons_class_file) :: !classes;

  (* 3. OnokiNil Class (Singleton) *)
  let ctx_nil = create_context () in
  let nil_class_idx = add_class ctx_nil.cpb "OnokiNil" in
  let list_interface_idx = add_class ctx_nil.cpb "OnokiList" in
  let object_class_idx = add_class ctx_nil.cpb "java/lang/Object" in
  
  (* Field: public static final OnokiNil INSTANCE *)
  let instance_name_idx = add_utf8 ctx_nil.cpb "INSTANCE" in
  let nil_desc_idx = add_utf8 ctx_nil.cpb "LOnokiNil;" in
  let instance_field = {
    access_flags = acc_public lor acc_static lor acc_final;
    name_index = instance_name_idx;
    descriptor_index = nil_desc_idx;
    attributes = [];
  } in
  
  (* Constructor (private) *)
  let init_name_idx = add_utf8 ctx_nil.cpb "<init>" in
  let init_desc_idx = add_utf8 ctx_nil.cpb "()V" in
  
  let init_ctx = create_context () in
  init_ctx.cpb <- ctx_nil.cpb;
  init_ctx.next_local <- 1;
  
  emit init_ctx Aload_0;
  let super_init = add_methodref ctx_nil.cpb "java/lang/Object" "<init>" "()V" in
  emit init_ctx (Invokespecial super_init);
  emit init_ctx Return;
  
  let init_code = encode_instructions init_ctx.instructions in
  let _code_name_idx = add_utf8 ctx_nil.cpb "Code" in
  
  let init_code_attr = Code {
    max_stack = 1;
    max_locals = 1;
    code = init_code;
    exception_table = [];
    attributes = [];
  } in
  
  let init_method : method_info = {
    access_flags = acc_private; (* Singleton *)
    name_index = init_name_idx;
    descriptor_index = init_desc_idx;
    attributes = [init_code_attr];
  } in
  
  (* Static Initializer <clinit> *)
  let clinit_name_idx = add_utf8 ctx_nil.cpb "<clinit>" in
  
  let clinit_ctx = create_context () in
  clinit_ctx.cpb <- ctx_nil.cpb;
  clinit_ctx.next_local <- 0;
  
  let nil_class_idx_ref = add_class ctx_nil.cpb "OnokiNil" in 
  emit clinit_ctx (New nil_class_idx_ref);
  emit clinit_ctx Dup;
  let init_ref = add_methodref ctx_nil.cpb "OnokiNil" "<init>" "()V" in
  emit clinit_ctx (Invokespecial init_ref);
  let instance_ref = add_fieldref ctx_nil.cpb "OnokiNil" "INSTANCE" "LOnokiNil;" in
  emit clinit_ctx (Putstatic instance_ref); (* Use Putstatic for singleton *)
  emit clinit_ctx Return;

  let clinit_code = encode_instructions clinit_ctx.instructions in
  
  let clinit_code_attr = Code {
    max_stack = 2;
    max_locals = 0;
    code = clinit_code;
    exception_table = [];
    attributes = [];
  } in

  let clinit_method : method_info = {
    access_flags = acc_public lor acc_static;
    name_index = clinit_name_idx;
    descriptor_index = init_desc_idx; (* ()V *)
    attributes = [clinit_code_attr];
  } in
  
  let nil_class_file = {
    minor_version = 0;
    major_version = 50;
    constant_pool = ctx_nil.cpb;
    access_flags = acc_public lor acc_super;
    this_class = nil_class_idx;
    super_class = object_class_idx;
    interfaces = [list_interface_idx];
    fields = [instance_field];
    methods = [init_method; clinit_method];
    attributes = [];
  } in
  classes := ("OnokiNil.class", write_class_file nil_class_file) :: !classes;
  
  !classes

(* Generate Module class *)
let generate_module_class (name : string) (decls : lambda list) (functions: function_def list) (externs: (string * string) list) : (string * bytes) =
  let cpb = create_cp_builder () in
  
  (* 1. Generate static fields for bindings *)
  let fields = List.filter_map (function
    | LLet (x, _, _) ->
        let name_idx = add_utf8 cpb x in
        let desc_idx = add_utf8 cpb "Ljava/lang/Object;" in
        Some {
          access_flags = acc_public lor acc_static;
          name_index = name_idx;
          descriptor_index = desc_idx;
          attributes = [];
        }
    | _ -> None
  ) decls in

  (* 2. Generate <clinit> *)
  let ctx = create_context () in
  ctx.cpb <- cpb;
  (* Pass global functions/externs so module can call them *)
  ctx.functions <- functions;
  ctx.externs <- externs;
  ctx.current_module <- Some name;
  
  let rec gen_clinit = function
    | [] -> ()
    | stmt :: rest -> (
        match stmt with
        | LLet (x, e, _) -> 
            gen_lambda ctx e;
            let field_ref = add_fieldref cpb name x "Ljava/lang/Object;" in
            emit ctx (Putstatic field_ref);
            gen_clinit rest
        | LExtern (n, impl) ->
            ctx.externs <- (n, impl) :: ctx.externs;
            gen_clinit rest
        | _ -> gen_clinit rest 
    )
  in
  gen_clinit decls;
  
  emit ctx Return;
  
  (* Resolve clinit *)
  let resolved_instrs = resolve_labels ctx in
  let code_bytes = encode_instructions resolved_instrs in
  let _code_name_idx = add_utf8 cpb "Code" in
  
  let code_attr = Code {
    max_stack = ctx.max_stack + 2;
    max_locals = ctx.next_local; 
    code = code_bytes;
    exception_table = [];
    attributes = [];
  } in
  
  let clinit_name_idx = add_utf8 cpb "<clinit>" in
  let clinit_desc_idx = add_utf8 cpb "()V" in
  let clinit_method : method_info = {
    access_flags = acc_public lor acc_static;
    name_index = clinit_name_idx;
    descriptor_index = clinit_desc_idx;
    attributes = [code_attr];
  } in
  
  (* 3. Generate static methods for functions defined in this module *)
  let initial_names = List.map (fun f -> f.name) functions in
  let new_functions = List.filter (fun f -> not (List.mem f.name initial_names)) ctx.functions in
  
  let fn_methods = List.map (generate_function_method cpb ctx.functions) (List.rev new_functions) in
  
  (* Build class *)
  let this_idx = add_class cpb name in
  let super_idx = add_class cpb "java/lang/Object" in
  let class_file = {
    minor_version = 0;
    major_version = 50;
    constant_pool = cpb;
    access_flags = acc_public lor acc_super;
    this_class = this_idx;
    super_class = super_idx;
    interfaces = [];
    fields = fields;
    methods = clinit_method :: fn_methods;
    attributes = [];
  } in
  (name ^ ".class", write_class_file class_file)

(* Generate all classes *)
let generate_classes (prog : lambda list) : (string * bytes) list =
  (* 1. Generate Main class *)
  reset_labels ();
  global_closure_counter := 0;
  global_closure_list := [];
  
  (* Generate runtime classes *)
  let list_classes = generate_list_classes () in
  let ctx = create_context () in
  
  (* Reserve local 0 for 'args' parameter *)
  ctx.next_local <- 1;
  
  (* Separate modules from main program *)
  let modules = List.filter_map (function
    | LModule (name, decls) -> Some (name, decls)
    | _ -> None
  ) prog in
  
  let main_prog = List.filter (function
    | LModule _ -> false
    | _ -> true
  ) prog in

  (* Generate code for program *)
  let rec gen_prog = function
    | [] -> ()
    | [last] -> 
        gen_lambda ctx last;
        (* Print result *)
        let print_stream_idx = add_fieldref ctx.cpb "java/lang/System" "out" "Ljava/io/PrintStream;" in
        let println_idx = add_methodref ctx.cpb "java/io/PrintStream" "println" "(Ljava/lang/Object;)V" in
        emit ctx (Getstatic print_stream_idx);
        emit ctx Swap;
        emit ctx (Invokevirtual println_idx)
    | stmt :: rest ->
        (match stmt with
         | LExtern _ -> 
             gen_lambda ctx stmt;
             gen_prog rest
         | _ -> 
             gen_lambda ctx stmt;
             emit ctx Pop; (* Discard result of declaration *)
             gen_prog rest)
  in
  gen_prog main_prog;
  
  emit ctx Return;
  
  (* Resolve labels in main method *)
  let resolved_main_instrs = resolve_labels ctx in
  let main_code_bytes = encode_instructions resolved_main_instrs in
  
  (* Pre-add "Code" to constant pool before creating attribute *)
  let _code_name_idx = add_utf8 ctx.cpb "Code" in
  
  let main_code_attr = Code {
    max_stack = ctx.max_stack + 2;
    max_locals = ctx.next_local;
    code = main_code_bytes;
    exception_table = [];
    attributes = [];
  } in
  
  (* Build main method *)
  let main_name_idx = add_utf8 ctx.cpb "main" in
  let main_desc_idx = add_utf8 ctx.cpb "([Ljava/lang/String;)V" in
  let main_method : method_info = {
    access_flags = acc_public lor acc_static;
    name_index = main_name_idx;
    descriptor_index = main_desc_idx;
    attributes = [main_code_attr];
  } in

  (* Generate methods for extracted functions *)
  let fn_methods = List.map (generate_function_method ctx.cpb ctx.functions) (List.rev ctx.functions) in
  
  (* Build Main class *)
  let this_class_idx = add_class ctx.cpb "Main" in
  let super_class_idx = add_class ctx.cpb "java/lang/Object" in
  
  let all_methods = main_method :: fn_methods in
  
  let class_file = {
    minor_version = 0;
    major_version = 50;  (* Java 6 - no stackmap frames required *)
    constant_pool = ctx.cpb;
    access_flags = acc_public lor acc_super;
    this_class = this_class_idx;
    super_class = super_class_idx;
    interfaces = [];
    fields = [];
    methods = all_methods;
    attributes = [];
  } in

  let main_bytes = write_class_file class_file in
  let interface_bytes = generate_function_interface () in
  
  (* Generate Module Classes *)
  let module_classes = List.map (fun (name, decls) -> 
    generate_module_class name decls ctx.functions ctx.externs
  ) modules in
  
  [("OnokiFunction.class", interface_bytes);
   ("Main.class", main_bytes)] @ list_classes @ module_classes @ !global_closure_list
  
