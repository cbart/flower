module Syntax.Abstract
where


import Syntax.Token (Const, Ident)
import Syntax.PrettyPrinter.Token


data Prog =
    Prog [Decl]

data Decl =
    Let [(Ident, Kind)] Ident Type Expr

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
  | TypeApp Type Type

data Kind =
    KindId
  | KindFun Kind Kind
