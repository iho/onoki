(* Main driver for Onoki compiler *)

let usage_msg = "onoki <file>"
let input_file = ref ""

let () =
  Arg.parse [] (fun f -> input_file := f) usage_msg;
  
  if !input_file = "" then begin
    Printf.eprintf "Error: No input file specified\n";
    Printf.eprintf "Usage: %s\n" usage_msg;
    exit 1
  end;

  (* Read source file *)
  let ic = open_in !input_file in
  let source = really_input_string ic (in_channel_length ic) in
  close_in ic;

  Printf.printf "=== Onoki Compiler ===\n\n";
  Printf.printf "Compiling: %s\n\n" !input_file;

  (* Lex & Parse *)
  let lexbuf = Lexing.from_string source in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = !input_file };
  
  try
    let ast = Onoki_lib.Parser.prog Onoki_lib.Lexer.token lexbuf in
    Printf.printf "✓ Parsing successful\n";
    Printf.printf "AST:\n%s\n\n" (Onoki_lib.Ast.show_program ast);

    (* Type check each declaration *)
    Printf.printf "=== Type Checking ===\n";
    List.iter (fun decl ->
      match decl with
      | Onoki_lib.Ast.DeclLet (name, expr) 
      | Onoki_lib.Ast.DeclLetRec (name, expr) -> (
          match Onoki_lib.Typing.type_check expr with
          | Some ty -> 
              Printf.printf "✓ %s : %s\n" name (Onoki_lib.Ast.show_ty ty)
          | None -> 
              Printf.printf "✗ %s : type error\n" name)
      | _ -> ()
    ) ast;
    Printf.printf "\n";

    (* Lower to Lambda IR *)
    Printf.printf "=== Lambda IR ===\n";
    let lambda_ir = Onoki_lib.Lambda.lower_program ast in
    List.iter (fun lam ->
      Printf.printf "%s\n" (Onoki_lib.Lambda.show_lambda lam)
    ) lambda_ir;
    Printf.printf "\n";

    Printf.printf "✓ Compilation completed successfully\n"

  with
  | Onoki_lib.Lexer.LexError msg ->
      Printf.eprintf "Lexical error: %s\n" msg;
      exit 1
  | Onoki_lib.Parser.Error ->
      let pos = lexbuf.lex_curr_p in
      Printf.eprintf "Parse error at line %d, column %d\n"
        pos.pos_lnum
        (pos.pos_cnum - pos.pos_bol + 1);
      exit 1
  | e ->
      Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string e);
      exit 1
