module Syntax.Abstract where


import Syntax.Token
import Syntax.PrettyPrinter.Token


type Poly = (Ident, Kind)

data Prog =
    Prog { runDecl :: [Decl] }

data Decl =
    Let [Poly] Ident Type Expr

data Expr =
    ExprFun [Ident] Expr
  | ExprIf Expr Expr Expr
  | ExprApp Expr Expr
  | ExprIdent Ident
  | ExprConst Const
  | ExprLoop

data Type =
    TypeId Ident
  | TypeFun Type Type
  | TypeApp Type Type deriving (Eq)

data Kind =
    KindId
  | KindFun Kind Kind
