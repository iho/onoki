(* Lambda IR - Intermediate representation for code generation *)
open Ast

(* Simplified Lambda IR *)
type lambda =
  | LConst of constant
  | LVar of string
  | LFun of string list * lambda
  | LApp of lambda * lambda list
  | LLet of string * lambda * lambda
  | LLetRec of (string * lambda) list * lambda
  | LIf of lambda * lambda * lambda
  | LSwitch of lambda * (int * lambda) list * lambda option
  | LTuple of lambda list
  | LField of lambda * int
  | LTry of lambda * (string * lambda)
  | LExtern of string * string
  | LModule of string * lambda list

and constant =
  | CInt of int
  | CFloat of float
  | CString of string
  | CBool of bool

(* Pretty printing *)
let show_constant = function
  | CInt i -> string_of_int i
  | CFloat f -> string_of_float f
  | CString s -> Printf.sprintf "\"%s\"" s
  | CBool b -> if b then "true" else "false"

let rec show_lambda = function
  | LConst c -> show_constant c
  | LVar x -> x
  | LFun (params, body) ->
      Printf.sprintf "(fun %s -> %s)" 
        (String.concat " " params) 
        (show_lambda body)
  | LApp (f, args) ->
      Printf.sprintf "(%s %s)" 
        (show_lambda f) 
        (String.concat " " (List.map show_lambda args))
  | LLet (x, e1, e2) ->
      Printf.sprintf "(let %s = %s in %s)" 
        x 
        (show_lambda e1) 
        (show_lambda e2)
  | LLetRec (bindings, body) ->
      let show_binding (name, expr) = 
        Printf.sprintf "%s = %s" name (show_lambda expr) 
      in
      Printf.sprintf "(let rec %s in %s)" 
        (String.concat " and " (List.map show_binding bindings))
        (show_lambda body)
  | LIf (cond, then_, else_) ->
      Printf.sprintf "(if %s then %s else %s)" 
        (show_lambda cond) 
        (show_lambda then_) 
        (show_lambda else_)
  | LSwitch (scrutinee, cases, default) ->
      let show_case (i, e) = Printf.sprintf "%d -> %s" i (show_lambda e) in
      let cases_str = String.concat " | " (List.map show_case cases) in
      let default_str = match default with
        | Some e -> " | _ -> " ^ show_lambda e
        | None -> ""
      in
      Printf.sprintf "(switch %s | %s%s)" 
        (show_lambda scrutinee) 
        cases_str 
        default_str
  | LTuple exprs ->
      Printf.sprintf "(%s)" 
        (String.concat ", " (List.map show_lambda exprs))
  | LField (e, i) ->
      Printf.sprintf "%s.%d" (show_lambda e) i
  | LTry (body, (exn, handler)) ->
      Printf.sprintf "(try %s with %s -> %s)" 
        (show_lambda body) 
        exn 
        (show_lambda handler)
  | LExtern (name, impl) ->
      Printf.sprintf "(extern %s = \"%s\")" name impl
  | LModule (name, decls) ->
      Printf.sprintf "(module %s {\n%s\n})" name
        (String.concat "\n" (List.map show_lambda decls))

(* Lowering from typed AST to Lambda form *)
let rec lower_expr : expr -> lambda = function
  | Int i -> LConst (CInt i)
  | Float f -> LConst (CFloat f)
  | String s -> LConst (CString s)
  | Bool b -> LConst (CBool b)
  | Var x -> LVar x
  | BinOp (op, e1, e2) ->
      let op_name = match op with
        | Add -> "__add"
        | Sub -> "__sub"
        | Mul -> "__mul"
        | Div -> "__div"
        | Mod -> "__mod"
        | Eq -> "__eq"
        | Ne -> "__ne"
        | Lt -> "__lt"
        | Le -> "__le"
        | Gt -> "__gt"
        | Ge -> "__ge"
        | And -> "__and"
        | Or -> "__or"
        | Concat -> "^" (* Concat matches ^ token, usually safe *)
      in
      LApp (LVar op_name, [lower_expr e1; lower_expr e2])
  | UnOp (op, e) ->
      let op_name = match op with
        | Not -> "__not"
        | Neg -> "__neg"
      in
      LApp (LVar op_name, [lower_expr e])
  | Fun (x, body) -> LFun ([x], lower_expr body)
  | App (f, arg) ->
      (match lower_expr f with
      | LApp (f', args) -> LApp (f', args @ [lower_expr arg])
      | f' -> LApp (f', [lower_expr arg]))
  | Let (x, e1, e2) -> LLet (x, lower_expr e1, lower_expr e2)
  | LetRec (x, e1, e2) -> LLetRec ([(x, lower_expr e1)], lower_expr e2)
  | If (cond, then_, else_) ->
      LIf (lower_expr cond, lower_expr then_, lower_expr else_)
  | Tuple exprs -> LTuple (List.map lower_expr exprs)
  | List exprs ->
      (* Build list as nested cons operations *)
      List.fold_right
        (fun e acc -> LApp (LVar "cons", [lower_expr e; acc]))
        exprs
        (LVar "nil")
  | Cons (e1, e2) ->
      LApp (LVar "cons", [lower_expr e1; lower_expr e2])
  | Match (scrutinee, cases) ->
      compile_match (lower_expr scrutinee) cases
  | Field (e, _field) ->
      (* TODO: track field positions *)
      LField (lower_expr e, 0)
  | Record fields ->
      LTuple (List.map (fun (_, e) -> lower_expr e) fields)
  | Variant (name, opt_expr) ->
      (* TODO: proper variant encoding *)
      (match opt_expr with
       | None -> LVar name
       | Some e -> LApp (LVar name, [lower_expr e]))
  | Seq exprs ->
      (* Sequence as nested let bindings *)
      List.fold_right
        (fun e acc -> LLet ("_", lower_expr e, acc))
        (List.rev (List.tl (List.rev exprs)))
        (lower_expr (List.hd (List.rev exprs)))

(* Pattern matching compilation to decision trees *)
and compile_match scrutinee cases : lambda =
  let counter = ref 0 in
  let fresh_name prefix =
    incr counter;
    Printf.sprintf "%s_%d" prefix !counter
  in
  
  let rec compile_pattern (scr : lambda) (pat : pattern) (next : lambda) (fail : lambda) : lambda =
    match pat with
    | PatWildcard -> next
    | PatVar x -> LLet (x, scr, next)
    | PatInt i ->
        LIf (LApp (LVar "__eq", [scr; LConst (CInt i)]),
             next,
             fail)
    | PatTuple pats ->
        (* Match field by field *)
        let rec match_fields i ps =
          match ps with
          | [] -> next
          | p :: rest ->
              let field_val = LField (scr, i) in
              (* Note: fail code is duplicated here. In a production compiler, 
                 we would use a join point (function) for failure. *)
              compile_pattern field_val p (match_fields (i + 1) rest) fail
        in
        match_fields 0 pats
    | PatCons (p1, p2) ->
        (* Check if list is non-empty cell *)
        (* Assuming runtime has is_empty or similar, or checking against Nil *)
        (* For now, using OnokiCons check. But we only have primitives.
           Let's assume we can check if it conforms to Cons. 
           Actually, primitives are: head, tail, is_empty.
           is_empty checks if it is Nil. *)
        LIf (LApp (LVar "is_empty", [scr]),
             fail,
             let head_val = LApp (LVar "head", [scr]) in
             let tail_val = LApp (LVar "tail", [scr]) in
             compile_pattern head_val p1 
               (compile_pattern tail_val p2 next fail) 
               fail)
    | PatList pats ->
        let rec match_list_pats (s : lambda) (ps : pattern list) : lambda =
          match ps with
          | [] -> 
              (* Check empty *)
              LIf (LApp (LVar "is_empty", [s]), next, fail)
          | p :: rest ->
               (* Check not empty *)
               LIf (LApp (LVar "is_empty", [s]),
                    fail,
                    let head_val = LApp (LVar "head", [s]) in
                    let tail_val = LApp (LVar "tail", [s]) in
                    compile_pattern head_val p
                      (match_list_pats tail_val rest)
                      fail)
        in
        match_list_pats scr pats
    | _ -> failwith "Unsupported pattern in lower (Record/Variant)"
  in

  let scrutinee_var = fresh_name "match_scr" in
  let scrutinee_ref = LVar scrutinee_var in
  
  let rec build_cases = function
    | [] -> LApp (LVar "failwith", [LConst (CString "Match failed")])
    | (pat, body) :: rest ->
        let fail_code = build_cases rest in
        (* Optimization: if pat is catch-all, don't generate fail code for this branch *)
        (* But compile_pattern generates it. *)
        compile_pattern scrutinee_ref pat (lower_expr body) fail_code
  in
  
  LLet (scrutinee_var, scrutinee, build_cases cases)

let rec lower_program : program -> lambda list = fun decls ->
  List.map (function
    | DeclLet (name, e) -> LLet (name, lower_expr e, LVar name)
    | DeclLetRec (name, e) -> LLetRec ([(name, lower_expr e)], LVar name)
    | DeclExtern (name, _ty, impl) -> LExtern (name, impl)
    | DeclModule (name, subdecls) -> LModule (name, lower_program subdecls)
    | _ -> failwith "TODO: other declarations"
  ) decls
