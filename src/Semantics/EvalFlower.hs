module Semantics.EvalFlower
( eval
) where

{-# LANGUAGE FlexibleInstances #-}

import Control.Monad

import Syntax.ErrM
import Syntax.AbsFlower

import Semantics.TypesFlower
import Semantics.EvalBase
import Semantics.Primitives
import Semantics.Error
import Semantics.State
import Semantics.TypeChecking

eval = evalProgram

type Evaluator a = a -> Evaluation a

evalProgram :: Program -> Err Program
evalProgram program = runEvaluation (programEval program) primitives

programEval :: Evaluator Program
programEval (Program decls) =
    liftM Program $ sequence $ do
        d <- decls
        return $ absDeclEval d

absDeclEval :: Evaluator AbstractDeclaration
absDeclEval absDecl@(ADLet decl) = do
    declEval decl
    return absDecl

declEval :: Evaluator Declaration
declEval decl@(DLet anIdent aType anExpr) = do
    check anExpr aType
    bindValue anIdent anExpr
    return decl
