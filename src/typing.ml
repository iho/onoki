(* Type inference engine for Onoki *)
open Ast

type env = (string * ty) list

let fresh_var_counter = ref 0

let fresh_var () =
  incr fresh_var_counter;
  Printf.sprintf "t%d" !fresh_var_counter

(* Simple type inference - will be extended with proper unification *)
let rec infer (env : env) (expr : expr) : ty option =
  match expr with
  | Int _ -> Some TyInt
  | Float _ -> Some TyFloat
  | String _ -> Some TyString
  | Bool _ -> Some TyBool
  | Var x -> List.assoc_opt x env
  | BinOp (op, e1, e2) -> (
      match op with
      | Add | Sub | Mul | Div | Mod -> (
          match infer env e1, infer env e2 with
          | Some TyInt, Some TyInt -> Some TyInt
          | Some TyFloat, Some TyFloat -> Some TyFloat
          | _ -> None)
      | Eq | Ne | Lt | Le | Gt | Ge -> (
          match infer env e1, infer env e2 with
          | Some t1, Some t2 when t1 = t2 -> Some TyBool
          | _ -> None)
      | And | Or -> (
          match infer env e1, infer env e2 with
          | Some TyBool, Some TyBool -> Some TyBool
          | _ -> None))
  | UnOp (Not, e) -> (
      match infer env e with
      | Some TyBool -> Some TyBool
      | _ -> None)
  | UnOp (Neg, e) -> (
      match infer env e with
      | Some TyInt -> Some TyInt
      | Some TyFloat -> Some TyFloat
      | _ -> None)
  | Fun (x, body) ->
      let arg_ty = TyVar (fresh_var ()) in
      let env' = (x, arg_ty) :: env in
      (match infer env' body with
       | Some ret_ty -> Some (TyFun (arg_ty, ret_ty))
       | None -> None)
  | App (f, arg) -> (
      match infer env f, infer env arg with
      | Some (TyFun (arg_ty, ret_ty)), Some arg_ty' 
        when arg_ty = arg_ty' -> Some ret_ty
      | _ -> None)
  | Let (x, e1, e2) -> (
      match infer env e1 with
      | Some ty1 ->
          let env' = (x, ty1) :: env in
          infer env' e2
      | None -> None)
  | LetRec (x, e1, e2) ->
      let rec_ty = TyVar (fresh_var ()) in
      let env' = (x, rec_ty) :: env in
      (match infer env' e1 with
       | Some ty1 when ty1 = rec_ty || true -> (* TODO: proper unification *)
           infer env' e2
       | _ -> None)
  | If (cond, then_, else_) -> (
      match infer env cond with
      | Some TyBool -> (
          match infer env then_, infer env else_ with
          | Some t1, Some t2 when t1 = t2 -> Some t1
          | _ -> None)
      | _ -> None)
  | Tuple exprs ->
      let rec infer_all = function
        | [] -> Some []
        | e :: es -> (
            match infer env e, infer_all es with
            | Some t, Some ts -> Some (t :: ts)
            | _ -> None)
      in
      (match infer_all exprs with
       | Some tys -> Some (TyTuple tys)
       | None -> None)
  | List [] -> Some (TyList (TyVar (fresh_var ())))
  | List (e :: es) -> (
      match infer env e with
      | Some elem_ty ->
          let rec check_all = function
            | [] -> true
            | e :: es -> (
                match infer env e with
                | Some t when t = elem_ty -> check_all es
                | _ -> false)
          in
          if check_all es then Some (TyList elem_ty) else None
      | None -> None)
  | Cons (e1, e2) -> (
      match infer env e1, infer env e2 with
      | Some elem_ty, Some (TyList list_ty) when elem_ty = list_ty ->
          Some (TyList elem_ty)
      | _ -> None)
  | _ -> None (* TODO: implement remaining cases *)

let type_check (expr : expr) : ty option =
  infer [] expr
