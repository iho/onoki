(* Type inference engine for Onoki - Hindley-Milner Algorithm W *)
open Ast

(* Type substitution *)
type subst = (string * ty) list

(* Type schemes for polymorphism *)
type ty_scheme = Forall of string list * ty

(* Environment bindings *)
type binding =
  | BindValue of ty_scheme
  | BindModule of env

and env = (string * binding) list

(* Fresh type variable counter *)
let fresh_var_counter = ref 0

let fresh_var () =
  incr fresh_var_counter;
  Printf.sprintf "'t%d" !fresh_var_counter

let reset_fresh_vars () =
  fresh_var_counter := 0

(* Apply substitution to a type *)
let rec apply_subst (s : subst) (t : ty) : ty =
  match t with
  | TyVar v -> (
      match List.assoc_opt v s with
      | Some t' -> apply_subst s t'  (* Follow substitution chains *)
      | None -> t)
  | TyFun (t1, t2) -> TyFun (apply_subst s t1, apply_subst s t2)
  | TyTuple ts -> TyTuple (List.map (apply_subst s) ts)
  | TyList t -> TyList (apply_subst s t)
  | TyVariant (name, ctors) ->
      TyVariant (name, List.map (fun (n, opt_t) ->
        (n, Option.map (apply_subst s) opt_t)) ctors)
  | TyRecord fields ->
      TyRecord (List.map (fun (n, t) -> (n, apply_subst s t)) fields)
  | _ -> t

(* Apply substitution to a type scheme *)
let apply_subst_scheme (s : subst) (Forall (vars, t) : ty_scheme) : ty_scheme =
  (* Remove bindings for quantified variables *)
  let s' = List.filter (fun (v, _) -> not (List.mem v vars)) s in
  Forall (vars, apply_subst s' t)

(* Apply substitution to a binding *)
let rec apply_subst_binding (s : subst) (b : binding) : binding =
  match b with
  | BindValue scheme -> BindValue (apply_subst_scheme s scheme)
  | BindModule env -> BindModule (List.map (fun (n, b) -> (n, apply_subst_binding s b)) env)

(* Compose two substitutions *)
let compose_subst (s1 : subst) (s2 : subst) : subst =
  (* Apply s1 to all types in s2, then append s1 *)
  List.map (fun (v, t) -> (v, apply_subst s1 t)) s2 @ s1

(* Occurs check - prevents infinite types *)
let rec occurs (v : string) (t : ty) : bool =
  match t with
  | TyVar v' -> v = v'
  | TyFun (t1, t2) -> occurs v t1 || occurs v t2
  | TyTuple ts -> List.exists (occurs v) ts
  | TyList t -> occurs v t
  | TyVariant (_, ctors) ->
      List.exists (fun (_, opt_t) ->
        match opt_t with Some t -> occurs v t | None -> false) ctors
  | TyRecord fields ->
      List.exists (fun (_, t) -> occurs v t) fields
  | _ -> false

(* Unification algorithm *)
let rec unify (t1 : ty) (t2 : ty) : subst option =
  match t1, t2 with
  (* Base types *)
  | TyInt, TyInt | TyFloat, TyFloat | TyString, TyString | TyBool, TyBool ->
      Some []
  
  (* Type variables *)
  | TyVar v1, TyVar v2 when v1 = v2 -> Some []
  | TyVar v, t | t, TyVar v ->
      if occurs v t then None  (* Occurs check *)
      else Some [(v, t)]
  
  (* Function types *)
  | TyFun (a1, r1), TyFun (a2, r2) -> (
      match unify a1 a2 with
      | None -> None
      | Some s1 ->
          let r1' = apply_subst s1 r1 in
          let r2' = apply_subst s1 r2 in
          (match unify r1' r2' with
           | None -> None
           | Some s2 -> Some (compose_subst s2 s1)))
  
  (* Tuple types *)
  | TyTuple ts1, TyTuple ts2 when List.length ts1 = List.length ts2 ->
      unify_list ts1 ts2
  
  (* List types *)
  | TyList t1, TyList t2 -> unify t1 t2
  
  (* Variant types *)
  | TyVariant (n1, _), TyVariant (n2, _) when n1 = n2 ->
      Some []  (* TODO: proper variant unification *)
  
  (* Record types *)
  | TyRecord fields1, TyRecord fields2 when List.length fields1 = List.length fields2 ->
      (* TODO: proper record unification *)
      Some []
  
  | _ -> None

and unify_list (ts1 : ty list) (ts2 : ty list) : subst option =
  match ts1, ts2 with
  | [], [] -> Some []
  | t1 :: ts1', t2 :: ts2' -> (
      match unify t1 t2 with
      | None -> None
      | Some s1 ->
          let ts1'' = List.map (apply_subst s1) ts1' in
          let ts2'' = List.map (apply_subst s1) ts2' in
          (match unify_list ts1'' ts2'' with
           | None -> None
           | Some s2 -> Some (compose_subst s2 s1)))
  | _ -> None

(* Free type variables in a type *)
let free_vars_ty (t : ty) : string list =
  let rec go acc = function
    | TyVar v -> if List.mem v acc then acc else v :: acc
    | TyFun (t1, t2) -> go (go acc t1) t2
    | TyTuple ts -> List.fold_left go acc ts
    | TyList t -> go acc t
    | TyVariant (_, ctors) ->
        List.fold_left (fun acc (_, opt_t) ->
          match opt_t with Some t -> go acc t | None -> acc) acc ctors
    | TyRecord fields ->
        List.fold_left (fun acc (_, t) -> go acc t) acc fields
    | _ -> acc
  in go [] t

(* Free type variables in a binding *)
let rec free_vars_binding (b : binding) : string list =
  match b with
  | BindValue (Forall (vars, t)) ->
      let fv = free_vars_ty t in
      List.filter (fun v -> not (List.mem v vars)) fv
  | BindModule env -> free_vars_env env

(* Free type variables in an environment *)
and free_vars_env (env : env) : string list =
  List.fold_left (fun acc (_, b) ->
    let fv = free_vars_binding b in
    List.fold_left (fun acc v -> if List.mem v acc then acc else v :: acc) acc fv
  ) [] env

(* Recursive lookup for module members *)
let lookup_var (env : env) (name : string) : ty_scheme option =
  if String.contains name '.' then
    let parts = String.split_on_char '.' name in
    let rec traverse_env current_env path =
      match path with
      | [] -> None
      | [x] -> (
          match List.assoc_opt x current_env with
          | Some (BindValue scheme) -> Some scheme
          | _ -> None)
      | m :: rest -> (
          match List.assoc_opt m current_env with
          | Some (BindModule inner_env) -> traverse_env inner_env rest
          | _ -> None)
    in
    traverse_env env parts
  else
    match List.assoc_opt name env with
    | Some (BindValue scheme) -> Some scheme
    | _ -> None

(* Generalization - create a type scheme *)
let generalize (env : env) (t : ty) : ty_scheme =
  let env_fv = free_vars_env env in
  let ty_fv = free_vars_ty t in
  let gen_vars = List.filter (fun v -> not (List.mem v env_fv)) ty_fv in
  Forall (gen_vars, t)

(* Instantiation - create a fresh instance of a type scheme *)
let instantiate (Forall (vars, t) : ty_scheme) : ty =
  let subst = List.map (fun v -> (v, TyVar (fresh_var ()))) vars in
  apply_subst subst t

(* Inference for patterns *)
let rec infer_pattern (env : env) (pat : pattern) (expected_ty : ty) : (env * subst) option =
  match pat with
  | PatWildcard -> Some (env, [])
  
  | PatVar x ->
      (* Add variable to environment with the expected type *)
      let binding = BindValue (Forall ([], expected_ty)) in
      Some ((x, binding) :: env, [])
      
  | PatInt _ ->
      (match unify expected_ty TyInt with
       | Some s -> Some (env, s)
       | None -> None)

  | PatTuple pats ->
      (* Create fresh types for elements *)
      let elem_tys = List.map (fun _ -> TyVar (fresh_var ())) pats in
      let tuple_ty = TyTuple elem_tys in
      (match unify expected_ty tuple_ty with
       | Some s ->
           let rec check_pats env acc_subst tys ps =
             match tys, ps with
             | [], [] -> Some (env, acc_subst)
             | t :: rest_tys, p :: rest_ps ->
                 let t' = apply_subst acc_subst t in
                 (match infer_pattern env p t' with
                  | Some (env', s') ->
                      let s_final = compose_subst s' acc_subst in
                      check_pats env' s_final rest_tys rest_ps
                  | None -> None)
             | _ -> None
           in
           check_pats env s elem_tys pats
       | None -> None)

  | PatList pats ->
       let elem_ty = TyVar (fresh_var ()) in
       let list_ty = TyList elem_ty in
       (match unify expected_ty list_ty with
        | Some s ->
            let elem_ty' = apply_subst s elem_ty in
            let rec check_list env acc_subst = function
              | [] -> Some (env, acc_subst)
              | p :: ps ->
                   let t_elem = apply_subst acc_subst elem_ty' in
                   (match infer_pattern env p t_elem with
                    | Some (env', s') ->
                        let s_next = compose_subst s' acc_subst in
                        check_list env' s_next ps
                    | None -> None)
            in
            check_list env s pats
        | None -> None)
  
  | PatCons (p1, p2) ->
       let elem_ty = TyVar (fresh_var ()) in
       let list_ty = TyList elem_ty in
       (match unify expected_ty list_ty with
        | Some s ->
            let elem_ty' = apply_subst s elem_ty in
            (match infer_pattern env p1 elem_ty' with
             | Some (env', s1) ->
                 let s_mid = compose_subst s1 s in
                 let list_ty' = apply_subst s_mid list_ty in
                 (match infer_pattern env' p2 list_ty' with
                  | Some (env'', s2) ->
                      Some (env'', compose_subst s2 s_mid)
                  | None -> None)
             | None -> None)
        | None -> None)

  | _ -> None (* TODO: other patterns *)

(* Main type inference function *)
let rec infer (env : env) (expr : expr) : (ty * subst) option =
  match expr with
  (* Literals *)
  | Int _ -> Some (TyInt, [])
  | Float _ -> Some (TyFloat, [])
  | String _ -> Some (TyString, [])
  | Bool _ -> Some (TyBool, [])
  
  (* Variables *)
  | Var x -> (
      match lookup_var env x with
      | Some scheme -> Some (instantiate scheme, [])
      | None -> None)
  
  (* Binary operators *)
  | BinOp (op, e1, e2) -> infer_binop env op e1 e2
  
  (* Unary operators *)
  | UnOp (Not, e) -> (
      match infer env e with
      | Some (t, s) ->
          (match unify t TyBool with
           | Some s' -> Some (TyBool, compose_subst s' s)
           | None -> None)
      | None -> None)
  
  | UnOp (Neg, e) -> (
      match infer env e with
      | Some (t, s) ->
          (* Try int first, then float *)
          (match unify t TyInt with
           | Some s' -> Some (TyInt, compose_subst s' s)
           | None ->
               (match unify t TyFloat with
                | Some s' -> Some (TyFloat, compose_subst s' s)
                | None -> None))
      | None -> None)
  
  (* Lambda abstraction *)
  | Fun (x, body) ->
      let arg_ty = TyVar (fresh_var ()) in
      let env' = (x, BindValue (Forall ([], arg_ty))) :: env in
      (match infer env' body with
       | Some (ret_ty, s) ->
           let arg_ty' = apply_subst s arg_ty in
           Some (TyFun (arg_ty', ret_ty), s)
       | None -> None)
  
  (* Function application *)
  | App (f, arg) ->
      (match infer env f with
       | Some (tf, s1) ->
           let env' = List.map (fun (v, binding) ->
             (v, apply_subst_binding s1 binding)) env in
           (match infer env' arg with
            | Some (targ, s2) ->
                let s = compose_subst s2 s1 in
                let tf' = apply_subst s tf in
                let ret_ty = TyVar (fresh_var ()) in
                (match unify tf' (TyFun (targ, ret_ty)) with
                 | Some s3 ->
                     let s_final = compose_subst s3 s in
                     Some (apply_subst s_final ret_ty, s_final)
                 | None -> None)
            | None -> None)
       | None -> None)
  
  (* Let binding *)
  | Let (x, e1, e2) ->
      (match infer env e1 with
       | Some (t1, s1) ->
           let env' = List.map (fun (v, binding) ->
             (v, apply_subst_binding s1 binding)) env in
           let scheme = generalize env' t1 in
           let env'' = (x, BindValue scheme) :: env' in
           (match infer env'' e2 with
            | Some (t2, s2) -> Some (t2, compose_subst s2 s1)
            | None -> None)
       | None -> None)
  
  (* Recursive let binding *)
  | LetRec (x, e1, e2) ->
      let rec_ty = TyVar (fresh_var ()) in
      let env' = (x, BindValue (Forall ([], rec_ty))) :: env in
      (match infer env' e1 with
       | Some (t1, s1) ->
           (match unify (apply_subst s1 rec_ty) t1 with
            | Some s2 ->
                let s = compose_subst s2 s1 in
                let env'' = List.map (fun (v, binding) ->
                  (v, apply_subst_binding s binding)) env in
                let final_ty = apply_subst s rec_ty in
                let scheme = generalize env'' final_ty in
                let env''' = (x, BindValue scheme) :: env'' in
                (match infer env''' e2 with
                 | Some (t2, s3) -> Some (t2, compose_subst s3 s)
                 | None -> None)
            | None -> None)
       | None -> None)
  
  (* If-then-else *)
  | If (cond, then_, else_) ->
      (match infer env cond with
       | Some (tcond, s1) ->
           (match unify tcond TyBool with
            | Some s2 ->
                let s = compose_subst s2 s1 in
                let env' = List.map (fun (v, binding) ->
                  (v, apply_subst_binding s binding)) env in
                (match infer env' then_ with
                 | Some (t1, s3) ->
                     let s' = compose_subst s3 s in
                     let env'' = List.map (fun (v, binding) ->
                       (v, apply_subst_binding s' binding)) env in
                     (match infer env'' else_ with
                      | Some (t2, s4) ->
                          let s'' = compose_subst s4 s' in
                          let t1' = apply_subst s'' t1 in
                          let t2' = apply_subst s'' t2 in
                          (match unify t1' t2' with
                           | Some s5 ->
                               let s_final = compose_subst s5 s'' in
                               Some (apply_subst s_final t1', s_final)
                           | None -> None)
                      | None -> None)
                 | None -> None)
            | None -> None)
       | None -> None)
  
  (* Tuples *)
  | Tuple exprs ->
      let rec infer_all env acc_types acc_subst = function
        | [] -> Some (List.rev acc_types, acc_subst)
        | e :: es ->
            let env' = List.map (fun (v, binding) ->
              (v, apply_subst_binding acc_subst binding)) env in
            (match infer env' e with
             | Some (t, s) ->
                 let s' = compose_subst s acc_subst in
                 infer_all env (t :: acc_types) s' es
             | None -> None)
      in
      (match infer_all env [] [] exprs with
       | Some (tys, s) -> Some (TyTuple tys, s)
       | None -> None)
  
  (* Lists *)
  | List [] ->
      let elem_ty = TyVar (fresh_var ()) in
      Some (TyList elem_ty, [])
  
  | List (e :: es) ->
      (match infer env e with
       | Some (elem_ty, s1) ->
           let rec check_all env acc_subst = function
             | [] -> Some acc_subst
             | e :: es ->
                 let env' = List.map (fun (v, binding) ->
                   (v, apply_subst_binding acc_subst binding)) env in
                 (match infer env' e with
                  | Some (t, s) ->
                      let s' = compose_subst s acc_subst in
                      let elem_ty' = apply_subst s' elem_ty in
                      let t' = apply_subst s' t in
                      (match unify elem_ty' t' with
                       | Some s'' ->
                           let s_final = compose_subst s'' s' in
                           check_all env s_final es
                       | None -> None)
                  | None -> None)
           in
           (match check_all env s1 es with
            | Some s_final ->
                let final_elem_ty = apply_subst s_final elem_ty in
                Some (TyList final_elem_ty, s_final)
            | None -> None)
       | None -> None)
  
  (* Cons *)
  | Cons (e1, e2) ->
      (match infer env e1 with
       | Some (elem_ty, s1) ->
           let env' = List.map (fun (v, binding) ->
             (v, apply_subst_binding s1 binding)) env in
           (match infer env' e2 with
            | Some (list_ty, s2) ->
                let s = compose_subst s2 s1 in
                let elem_ty' = apply_subst s elem_ty in
                (match unify list_ty (TyList elem_ty') with
                 | Some s3 ->
                     let s_final = compose_subst s3 s in
                     Some (apply_subst s_final (TyList elem_ty'), s_final)
                 | None -> None)
            | None -> None)
       | None -> None)

  (* Pattern Match *)
  | Match (scrutinee, cases) ->
      (match infer env scrutinee with
       | Some (t_scr, s_scr) ->
           let ret_ty = TyVar (fresh_var ()) in
           
           let rec check_cases acc_subst = function
             | [] -> Some (apply_subst acc_subst ret_ty, acc_subst)
             | (pat, body) :: rest ->
                 let t_scr' = apply_subst acc_subst t_scr in
                 (match infer_pattern env pat t_scr' with
                  | Some (env_pat, s_pat) ->
                      let s_case = compose_subst s_pat acc_subst in
                      (* Apply subst to environment so body sees constraints *)
                      let env_body = List.map (fun (v, b) -> (v, apply_subst_binding s_case b)) env_pat in
                      
                      (match infer env_body body with
                       | Some (t_body, s_body) ->
                           let s_final_case = compose_subst s_body s_case in
                           let ret_ty' = apply_subst s_final_case ret_ty in
                           let t_body' = apply_subst s_final_case t_body in
                           
                           (match unify ret_ty' t_body' with
                            | Some s_unify ->
                                let s_res = compose_subst s_unify s_final_case in
                                check_cases s_res rest
                            | None -> None)
                       | None -> None)
                  | None -> None)
           in
           check_cases s_scr cases
       | None -> None)

  (* Other cases - TODO *)
  | Variant _ | Record _ | Field _ | Seq _ -> None

(* Type inference for binary operators *)
and infer_binop (env : env) (op : binop) (e1 : expr) (e2 : expr) : (ty * subst) option =
  match op with
  | Add | Sub | Mul | Div | Mod ->
      (* Arithmetic: int op int -> int OR float op float -> float *)
      (match infer env e1, infer env e2 with
       | Some (t1, s1), Some (t2, s2) ->
           let s = compose_subst s2 s1 in
           let t1' = apply_subst s t1 in
           let t2' = apply_subst s t2 in
           (* Try int first *)
           (match unify t1' TyInt with
            | Some s3 ->
                let s' = compose_subst s3 s in
                (match unify (apply_subst s' t2') TyInt with
                 | Some s4 -> Some (TyInt, compose_subst s4 s')
                 | None -> None)
            | None ->
                (* Try float *)
                (match unify t1' TyFloat with
                 | Some s3 ->
                     let s' = compose_subst s3 s in
                     (match unify (apply_subst s' t2') TyFloat with
                      | Some s4 -> Some (TyFloat, compose_subst s4 s')
                      | None -> None)
                 | None -> None))
       | _ -> None)
  
  | Eq | Ne | Lt | Le | Gt | Ge ->
      (* Comparison: 'a op 'a -> bool *)
      (match infer env e1, infer env e2 with
       | Some (t1, s1), Some (t2, s2) ->
           let s = compose_subst s2 s1 in
           let t1' = apply_subst s t1 in
           let t2' = apply_subst s t2 in
           (match unify t1' t2' with
            | Some s3 -> Some (TyBool, compose_subst s3 s)
            | None -> None)
       | _ -> None)
  
  | And | Or ->
      (* Logical: bool op bool -> bool *)
      (match infer env e1, infer env e2 with
       | Some (t1, s1), Some (t2, s2) ->
           let s = compose_subst s2 s1 in
           (match unify (apply_subst s t1) TyBool with
            | Some s3 ->
                let s' = compose_subst s3 s in
                (match unify (apply_subst s' t2) TyBool with
                 | Some s4 -> Some (TyBool, compose_subst s4 s')
                 | None -> None)
            | None -> None)
       | _ -> None)

  | Concat ->
      (* String: string ^ string -> string *)
      (match infer env e1, infer env e2 with
       | Some (t1, s1), Some (t2, s2) ->
           let s = compose_subst s2 s1 in
           (match unify (apply_subst s t1) TyString with
            | Some s3 ->
                let s' = compose_subst s3 s in
                (match unify (apply_subst s' t2) TyString with
                 | Some s4 -> Some (TyString, compose_subst s4 s')
                 | None -> None)
            | None -> None)
       | _ -> None)


(* Type check a single declaration *)
let rec type_check_decl (env : env) (decl : decl) : ((string * binding) * env) option =
  match decl with
  | DeclLet (name, expr) -> (
      match infer env expr with
      | Some (ty, s) ->
          let env' = List.map (fun (v, binding) ->
            (v, apply_subst_binding s binding)) env in
          let scheme = generalize env' ty in
          let binding = BindValue scheme in
          let env'' = (name, binding) :: env' in
          Some ((name, binding), env'')
      | None -> None)
  
  | DeclLetRec (name, expr) ->
      let rec_ty = TyVar (fresh_var ()) in
      let env' = (name, BindValue (Forall ([], rec_ty))) :: env in
      (match infer env' expr with
       | Some (ty, s) ->
           (match unify (apply_subst s rec_ty) ty with
            | Some s2 ->
                let s_final = compose_subst s2 s in
                let env'' = List.map (fun (v, binding) ->
                  (v, apply_subst_binding s_final binding)) env in
                let final_ty = apply_subst s_final rec_ty in
                let scheme = generalize env'' final_ty in
                let binding = BindValue scheme in
                let env''' = (name, binding) :: env'' in
                Some ((name, binding), env''')
            | None -> None)
       | None -> None)
  
  | DeclExtern (name, ty, _impl) ->
      (* External declaration: trust the type *)
      let scheme = generalize env ty in
      let binding = BindValue scheme in
      Some ((name, binding), (name, binding) :: env)

  | DeclModule (name, decls) ->
      (* Type check module body using current env for lookups *)
      (* We accumulate local bindings for the module environment *)
      let rec check_module_decls local_env rest_decls =
         match rest_decls with
         | [] -> Some local_env
         | d :: ds ->
             (* Check d in (local_env + env) *)
             let check_env = local_env @ env in
             (match type_check_decl check_env d with
              | Some ((n, b), _unused_env) ->
                  check_module_decls ((n, b) :: local_env) ds
              | None -> None)
      in
      (match check_module_decls [] decls with
       | Some local_env ->
           (* local_env has newest first, which is correct for env *)
           let mod_binding = BindModule local_env in
           Some ((name, mod_binding), (name, mod_binding) :: env)
       | None -> None)
  
  | DeclType _ -> 
       (* Types are handled separately or during resolution, ignoring for now *)
       Some (("", BindValue (Forall([], TyTuple[]))), env) 

(* Initial environment with built-ins *)
let initial_env : env = [
  ("add", BindValue (Forall ([], TyFun (TyInt, TyFun (TyInt, TyInt)))));
  ("sub", BindValue (Forall ([], TyFun (TyInt, TyFun (TyInt, TyInt)))));
  ("mul", BindValue (Forall ([], TyFun (TyInt, TyFun (TyInt, TyInt)))));
  ("div", BindValue (Forall ([], TyFun (TyInt, TyFun (TyInt, TyInt)))));
  ("mod", BindValue (Forall ([], TyFun (TyInt, TyFun (TyInt, TyInt)))));
  ("eq", BindValue (Forall ([], TyFun (TyInt, TyFun (TyInt, TyBool)))));
  ("ne", BindValue (Forall ([], TyFun (TyInt, TyFun (TyInt, TyBool)))));
  ("lt", BindValue (Forall ([], TyFun (TyInt, TyFun (TyInt, TyBool)))));
  ("le", BindValue (Forall ([], TyFun (TyInt, TyFun (TyInt, TyBool)))));
  ("gt", BindValue (Forall ([], TyFun (TyInt, TyFun (TyInt, TyBool)))));
  ("ge", BindValue (Forall ([], TyFun (TyInt, TyFun (TyInt, TyBool)))));
  ("not", BindValue (Forall ([], TyFun (TyBool, TyBool))));
  (* List primitives *)
  ("cons", BindValue (Forall (["'a"], TyFun (TyVar "'a", TyFun (TyList (TyVar "'a"), TyList (TyVar "'a"))))));
  ("head", BindValue (Forall (["'a"], TyFun (TyList (TyVar "'a"), TyVar "'a"))));
  ("tail", BindValue (Forall (["'a"], TyFun (TyList (TyVar "'a"), TyList (TyVar "'a")))));
  ("is_empty", BindValue (Forall (["'a"], TyFun (TyList (TyVar "'a"), TyBool))));
]

(* Type check an entire program *)
let type_check_program (prog : program) : (string * binding) list option =
  reset_fresh_vars ();
  let rec go env acc = function
    | [] -> Some (List.rev acc)
    | decl :: rest ->
        (match type_check_decl env decl with
         | Some ((name, binding), env') -> go env' ((name, binding) :: acc) rest
         | None -> None)
  in
  go initial_env [] prog

(* Simple type check for single expression (for backward compatibility) *)
let type_check (expr : expr) : ty option =
  reset_fresh_vars ();
  match infer initial_env expr with
  | Some (ty, _) -> Some ty
  | None -> None
