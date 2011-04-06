module Semantics.TypeChecking
( check
, deduceType
, typeName
) where

import Syntax.AbsFlower

import Semantics.EvalBase
import Semantics.Error
import Semantics.State


check :: Expr -> Type -> Evaluation ()
check anExpr aType = do
    expected <- typeName aType
    actual <- deduceType anExpr
    if expected == actual then return () else typeError expected actual

typeName :: Type -> Evaluation String
typeName (TypeId (Ident aName)) = return aName

deduceType :: Expr -> Evaluation String
deduceType (ExprConst _) = return "Int"
deduceType (ExprId identifier) = getValue identifier >>= deduceType
