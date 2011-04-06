module Semantics.EvalFlower
( eval
) where

{-# LANGUAGE FlexibleInstances #-}

import Control.Monad
import Control.Monad.State
import Control.Monad.Identity

import Syntax.ErrM
import Syntax.AbsFlower

import Semantics.TypesFlower
import Semantics.EvalBase
import Semantics.Primitives
import Semantics.Error
import Semantics.State
import Semantics.TypeChecking

eval = evalProgram

evalProgram :: Program -> Err Program
evalProgram program = evalStateT (programEvaluator program) primitives

programEvaluator :: Program -> Evaluation Program
programEvaluator (Program declarations) =
    liftM Program $ sequence $ do
        d <- declarations
        return $ abstractDeclarationEvaluator d

abstractDeclarationEvaluator :: AbstractDeclaration -> Evaluation AbstractDeclaration
abstractDeclarationEvaluator abstractDeclaration@(ADLet aDeclaration) = do
    declarationEvaluator aDeclaration
    return abstractDeclaration

declarationEvaluator :: Declaration -> Evaluation Declaration
declarationEvaluator declaration@(DLet anIdentifier aType anExpression) = do
    check anExpression aType
    bindValue anIdentifier anExpression
    return declaration
