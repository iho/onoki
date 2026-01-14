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
  | LRecordDef of string * (string * string) list (* Name, (Field, Descriptor) *)
  | LNewRecord of string * lambda list (* Name, Args *)
  | LGetField of lambda * string * string (* Expr, Class, Field *)
  | LNewVariant of string * lambda list (* Name, Args *)
  | LInstanceof of lambda * string (* Expr, Class *)
  | LCheckCast of lambda * string (* Expr, Class *)
  | LVariantDef of string * (string * string option) list (* TypeName, (CtorName, ArgDesc option) *)

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
  | LRecordDef (name, fields) ->
      let show_field (n, t) = Printf.sprintf "%s:%s" n t in
      Printf.sprintf "(record %s { %s })" name (String.concat "; " (List.map show_field fields))
  | LNewRecord (name, args) ->
      Printf.sprintf "(new %s %s)" name (String.concat " " (List.map show_lambda args))
  | LNewVariant (name, args) ->
      Printf.sprintf "(variant %s %s)" name (String.concat " " (List.map show_lambda args))
  | LGetField (e, cls, f) ->
      Printf.sprintf "%s.%s::%s" (show_lambda e) cls f
  | LInstanceof (e, cls) ->
      Printf.sprintf "(%s instanceof %s)" (show_lambda e) cls
  | LCheckCast (e, cls) ->
      Printf.sprintf "((%s)%s)" cls (show_lambda e)
  | LVariantDef (name, ctors) ->
      let show_ctor (n, arg) = Printf.sprintf "%s%s" n (match arg with Some t -> "(" ^ t ^ ")" | None -> "") in
      Printf.sprintf "(type %s = %s)" name (String.concat " | " (List.map show_ctor ctors))

(* Lowering type to JVM descriptor *)
let lower_type : ty -> string = function
  | TyInt -> "Ljava/lang/Object;"
  | TyFloat -> "Ljava/lang/Object;"
  | TyBool | TyString | TyList _ | TyTuple _ | TyFun _ | TyVar _ -> "Ljava/lang/Object;" (* Boxed/Unknown *)
  | TyRecord (Some name, _) -> "L" ^ name ^ ";" 
  | TyRecord (None, _) -> "Ljava/lang/Object;" 
  | TyVariant (name, _) -> "L" ^ name ^ ";"

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
        | Concat -> "^"
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
      List.fold_right
        (fun e acc -> LApp (LVar "cons", [lower_expr e; acc]))
        exprs
        (LVar "nil")
  | Cons (e1, e2) ->
      LApp (LVar "cons", [lower_expr e1; lower_expr e2])
  | Match (scrutinee, cases) ->
      compile_match (lower_expr scrutinee) cases
  | Field (e, field, class_ref) ->
      (* Handle Module.field access (parsed as Field(Variant("Module", None), "field")) *)
      (match e with
       | Variant (mod_name, None) ->
           (* This is actually a qualified module access: Module.field *)
           LVar (mod_name ^ "." ^ field)
       | _ ->
           (* Use captured class name from type checker if available *)
           (match !class_ref with
            | Some cls -> LGetField (lower_expr e, cls, field)
            | None -> 
                (* Fallback for tests/untyped paths or structural? *)
                LField (lower_expr e, 0)))
  | Record (name_ref, fields) ->
       (match !name_ref with
        | Some name ->
            let sorted_fields = List.sort (fun (a,_) (b,_) -> String.compare a b) fields in
            let args = List.map (fun (_, e) -> lower_expr e) sorted_fields in
            LNewRecord (name, args)
        | None -> 
            LTuple (List.map (fun (_, e) -> lower_expr e) fields))
  | Variant (name, opt_expr) ->
      (match opt_expr with
       | None -> LNewVariant (name, [])
       | Some e -> LNewVariant (name, [lower_expr e]))
  | Seq exprs ->
      List.fold_right
        (fun e acc -> LLet ("_", lower_expr e, acc))
        (List.rev (List.tl (List.rev exprs)))
        (lower_expr (List.hd (List.rev exprs)))

(* ... compile_match ... *)

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
              compile_pattern field_val p (match_fields (i + 1) rest) fail
        in
        match_fields 0 pats
    | PatCons (p1, p2) ->
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
              LIf (LApp (LVar "is_empty", [s]), next, fail)
          | p :: rest ->
               LIf (LApp (LVar "is_empty", [s]),
                    fail,
                    let head_val = LApp (LVar "head", [s]) in
                    let tail_val = LApp (LVar "tail", [s]) in
                    compile_pattern head_val p
                      (match_list_pats tail_val rest)
                      fail)
        in
        match_list_pats scr pats
        
    | PatVariant (cname, subpat_opt) ->
        LIf (LInstanceof (scr, cname),
             (match subpat_opt with
              | Some p -> 
                   (* Cast to Constructor class *)
                   let cast_scr = LCheckCast (scr, cname) in
                   (* Access value field *)
                   let val_field = LGetField (cast_scr, cname, "value") in
                   compile_pattern val_field p next fail
              | None -> next),
             fail)

    | _ -> failwith "Unsupported pattern in lower (Record)"
  in

  let scrutinee_var = fresh_name "match_scr" in
  let scrutinee_ref = LVar scrutinee_var in
  
  let rec build_cases = function
    | [] -> LApp (LVar "failwith", [LConst (CString "Match failed")])
    | (pat, body) :: rest ->
        let fail_code = build_cases rest in
        compile_pattern scrutinee_ref pat (lower_expr body) fail_code
  in
  
  LLet (scrutinee_var, scrutinee, build_cases cases)

let rec lower_program : program -> lambda list = fun decls ->
  List.map (function
    | DeclLet (name, e) -> LLet (name, lower_expr e, LVar name)
    | DeclLetRec (name, e) -> LLetRec ([(name, lower_expr e)], LVar name)
    | DeclExtern (name, _ty, impl) -> LExtern (name, impl)
    | DeclModule (name, subdecls) -> LModule (name, lower_program subdecls)
    | DeclType (name, TyRecord (_, fields)) ->
         let sorted_fields = List.sort (fun (a,_) (b,_) -> String.compare a b) fields in
         let desc_fields = List.map (fun (n, t) -> (n, lower_type t)) sorted_fields in
         LRecordDef (name, desc_fields)
    | DeclType (name, TyVariant (_, ctors)) ->
         let desc_ctors = List.map (fun (n, t_opt) -> 
           (n, Option.map lower_type t_opt)) ctors in
         LVariantDef (name, desc_ctors)
    | DeclType _ -> LTuple [] (* Ignore other types *)
  ) decls
