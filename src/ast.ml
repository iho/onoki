(* Core AST definitions for Onoki language *)

type ty =
  | TyInt
  | TyFloat
  | TyString
  | TyBool
  | TyVar of string
  | TyFun of ty * ty
  | TyTuple of ty list
  | TyList of ty
  | TyVariant of string * (string * ty option) list
  | TyRecord of (string * ty) list
  [@@deriving show, eq]

type binop = 
  | Add | Sub | Mul | Div | Mod 
  | Eq | Ne | Lt | Le | Gt | Ge 
  | And | Or
  [@@deriving show, eq]

type unop = 
  | Not 
  | Neg
  [@@deriving show, eq]

type expr =
  | Int of int
  | Float of float
  | String of string
  | Bool of bool
  | Var of string
  | BinOp of binop * expr * expr
  | UnOp of unop * expr
  | If of expr * expr * expr
  | Fun of string * expr
  | App of expr * expr
  | Let of string * expr * expr
  | LetRec of string * expr * expr
  | Match of expr * (pattern * expr) list
  | Tuple of expr list
  | List of expr list
  | Cons of expr * expr
  | Variant of string * expr option
  | Record of (string * expr) list
  | Field of expr * string
  | Seq of expr list
  [@@deriving show, eq]

and pattern =
  | PatInt of int
  | PatVar of string
  | PatWildcard
  | PatTuple of pattern list
  | PatCons of pattern * pattern
  | PatVariant of string * pattern option
  | PatRecord of (string * pattern) list
  [@@deriving show, eq]

type decl =
  | DeclLet of string * expr
  | DeclLetRec of string * expr
  | DeclType of string * ty
  | DeclModule of string * decl list
  | DeclExtern of string * ty
  [@@deriving show, eq]

type program = decl list
  [@@deriving show, eq]
