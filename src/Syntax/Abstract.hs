module Syntax.Abstract
where


import Syntax.Token (Const, Ident)


data Prog =
    Prog [Decl]
    deriving (Show)

data Decl =
    Let [(Ident, Kind)] Ident Type Expr
    deriving (Show)

data Expr =
    ExprFun [Ident] Expr
  | ExprIf Expr Expr Expr
  | ExprApp Expr Expr
  | ExprIdent Ident
  | ExprConst Const
  | ExprLoop
    deriving (Show)

data Type =
    TypeId Ident
  | TypeFun Type Type
  | TypeApp Type Type
    deriving (Show)

data Kind =
    KindId
  | KindFun Kind Kind
    deriving (Show)
