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

(* Lowering from typed AST to Lambda form *)
let rec lower_expr : expr -> lambda = function
  | Int i -> LConst (CInt i)
  | Float f -> LConst (CFloat f)
  | String s -> LConst (CString s)
  | Bool b -> LConst (CBool b)
  | Var x -> LVar x
  | BinOp (op, e1, e2) ->
      let op_name = match op with
        | Add -> "add"
        | Sub -> "sub"
        | Mul -> "mul"
        | Div -> "div"
        | Mod -> "mod"
        | Eq -> "eq"
        | Ne -> "ne"
        | Lt -> "lt"
        | Le -> "le"
        | Gt -> "gt"
        | Ge -> "ge"
        | And -> "and"
        | Or -> "or"
      in
      LApp (LVar op_name, [lower_expr e1; lower_expr e2])
  | UnOp (op, e) ->
      let op_name = match op with
        | Not -> "not"
        | Neg -> "neg"
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
  (* Simple linear search - production would optimize to jump tables *)
  let rec build_tree = function
    | [] -> failwith "Non-exhaustive pattern match"
    | [(PatWildcard, e)] -> lower_expr e
    | [(PatVar x, e)] -> LLet (x, scrutinee, lower_expr e)
    | [(PatInt i, e)] ->
        LIf (LApp (LVar "eq", [scrutinee; LConst (CInt i)]),
             lower_expr e,
             failwith "Non-exhaustive pattern match")
    | (PatInt i, e) :: rest ->
        LIf (LApp (LVar "eq", [scrutinee; LConst (CInt i)]),
             lower_expr e,
             build_tree rest)
    | (PatVar x, e) :: _ ->
        LLet (x, scrutinee, lower_expr e)
    | (PatWildcard, e) :: _ ->
        lower_expr e
    | _ -> failwith "TODO: complex pattern matching"
  in
  build_tree cases

let lower_program : program -> lambda list = fun decls ->
  List.map (function
    | DeclLet (name, e) -> LLet (name, lower_expr e, LVar name)
    | DeclLetRec (name, e) -> LLetRec ([(name, lower_expr e)], LVar name)
    | _ -> failwith "TODO: other declarations"
  ) decls
