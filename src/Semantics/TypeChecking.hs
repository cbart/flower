module Semantics.TypeChecking
( check
) where

import Syntax.AbsFlower

import Semantics.EvalBase
import Semantics.Error
import Semantics.State

check :: Expr -> Type -> Evaluation Type
check anExpr expected = do
    actual <- deduceType anExpr
    typesEqual expected actual

deduceType :: Expr -> Evaluation Type
deduceType (ExprConst _) = return $ simpleType "Int"
deduceType (ExprId identifier) = getValue identifier >>= deduceType
deduceType (ExprIf boolExpr thenExpr elseExpr) = do
    check boolExpr $ simpleType "Bool"
    thenType <- deduceType thenExpr
    elseType <- deduceType elseExpr
    typesEqual thenType elseType

typesEqual :: Type -> Type -> Evaluation Type
typesEqual expected actual =
    if expected == actual then return expected else typeMismatchError expected actual

simpleType :: String -> Type
simpleType = TypeId . Ident
