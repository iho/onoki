(* Main driver for Onoki compiler *)

let usage_msg = "onoki <file>"
let input_file = ref ""

(* Track already-included files to prevent cycles *)
let included_files : (string, bool) Hashtbl.t = Hashtbl.create 16

(* Parse a single file and return its AST *)
let parse_file (filename : string) : Onoki_lib.Ast.program =
  let ic = open_in filename in
  let source = really_input_string ic (in_channel_length ic) in
  close_in ic;
  let lexbuf = Lexing.from_string source in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  Onoki_lib.Parser.prog Onoki_lib.Lexer.token lexbuf

(* Resolve include paths relative to the including file's directory *)
let resolve_include_path (base_file : string) (include_path : string) : string =
  if Filename.is_relative include_path then
    Filename.concat (Filename.dirname base_file) include_path
  else
    include_path

(* Get canonical path for cycle detection *)
let canonical_path (path : string) : string =
  (* Simple normalization without Unix module *)
  let abs_path = 
    if Filename.is_relative path then
      Filename.concat (Sys.getcwd ()) path
    else
      path
  in
  (* Basic normalization by removing . and .. components *)
  abs_path

(* Recursively resolve all includes in a program *)
let rec resolve_includes (base_file : string) (decls : Onoki_lib.Ast.program) : Onoki_lib.Ast.program =
  List.concat_map (fun decl ->
    match decl with
    | Onoki_lib.Ast.DeclInclude path ->
        let resolved_path = resolve_include_path base_file path in
        let canon = canonical_path resolved_path in
        if Hashtbl.mem included_files canon then
          [] (* Already included, skip to prevent cycles *)
        else begin
          Hashtbl.add included_files canon true;
          if not (Sys.file_exists resolved_path) then
            failwith (Printf.sprintf "Include file not found: %s (resolved from %s)" path resolved_path);
          let included_ast = parse_file resolved_path in
          resolve_includes resolved_path included_ast
        end
    | Onoki_lib.Ast.DeclModule (name, inner_decls) ->
        (* Recursively resolve includes within modules *)
        [Onoki_lib.Ast.DeclModule (name, resolve_includes base_file inner_decls)]
    | other -> [other]
  ) decls

let () =
  Arg.parse [] (fun f -> input_file := f) usage_msg;
  
  if !input_file = "" then begin
    Printf.eprintf "Error: No input file specified\n";
    Printf.eprintf "Usage: %s\n" usage_msg;
    exit 1
  end;

  Printf.printf "=== Onoki Compiler ===\n\n";
  Printf.printf "Compiling: %s\n\n" !input_file;

  try
    (* Parse main file *)
    let raw_ast = parse_file !input_file in
    
    (* Mark main file as included *)
    Hashtbl.add included_files (canonical_path !input_file) true;
    
    (* Resolve all includes *)
    let ast = resolve_includes !input_file raw_ast in
    
    Printf.printf "✓ Parsing successful\n";
    Printf.printf "AST:\n%s\n\n" (Onoki_lib.Ast.show_program ast);

    (* Type check the entire program *)
    Printf.printf "=== Type Checking ===\n";
    (match Onoki_lib.Typing.type_check_program ast with
     | Some bindings ->
         List.iter (fun (name, binding) ->
           match binding with
           | Onoki_lib.Typing.BindValue (Onoki_lib.Typing.Forall (_, ty)) ->
               Printf.printf "✓ %s : %s\n" name (Onoki_lib.Ast.show_ty ty)
           | Onoki_lib.Typing.BindModule _ ->
               Printf.printf "✓ module %s\n" name
           | Onoki_lib.Typing.BindType ty ->
               Printf.printf "✓ type %s = %s\n" name (Onoki_lib.Ast.show_ty ty)
         ) bindings;
         Printf.printf "\n"
     | None ->
         Printf.printf "✗ Type checking failed\n\n");

    (* Lower to Lambda IR *)
    Printf.printf "=== Lambda IR ===\n";
    let lambda_ir = Onoki_lib.Lambda.lower_program ast in
    List.iter (fun lam ->
      Printf.printf "%s\n" (Onoki_lib.Lambda.show_lambda lam)
    ) lambda_ir;
    Printf.printf "\n";

    (* Generate JVM bytecode *)
    Printf.printf "=== JVM Bytecode Generation ===\n";
    Printf.printf "=== JVM Bytecode Generation ===\n";
    let classes = Onoki_lib.Jvmgen.generate_classes lambda_ir in
    List.iter (fun (filename, bytecode) ->
      let oc = open_out_bin filename in
      output_bytes oc bytecode;
      close_out oc;
      Printf.printf "✓ Generated %s (%d bytes)\n" filename (Bytes.length bytecode)
    ) classes;
    Printf.printf "\n";

    Printf.printf "✓ Compilation completed successfully\n";
    Printf.printf "\nRun with: java Main\n"

  with
  | Onoki_lib.Lexer.LexError msg ->
      Printf.eprintf "Lexical error: %s\n" msg;
      exit 1
  | Onoki_lib.Parser.Error ->
      Printf.eprintf "Parse error in %s\n" !input_file;
      exit 1
  | Failure msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | e ->
      Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string e);
      exit 1
