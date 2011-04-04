module Semantics.EvalFlower
( eval
) where

{-# LANGUAGE FlexibleInstances #-}

import Prelude hiding ( lookup )
import Data.Map
import Control.Monad
import Control.Monad.State
import Control.Monad.Identity

import Syntax.ErrM
import Syntax.AbsFlower

import Semantics.TypesFlower


type Bindings = Map Ident Value
type Computation a = StateT Bindings Err a

eval = evalProgram

evalProgram :: Program -> Err Program
evalProgram program = evalStateT (programEvaluator program) baseBindings

baseBindings :: Bindings
baseBindings = empty

programEvaluator :: Program -> Computation Program
programEvaluator (Program declarations) =
    liftM Program $ sequence $ do
        d <- declarations
        return $ abstractDeclarationEvaluator d

abstractDeclarationEvaluator :: AbstractDeclaration -> Computation AbstractDeclaration
abstractDeclarationEvaluator abstractDeclaration@(ADLet aDeclaration) = do
    declarationEvaluator aDeclaration
    return abstractDeclaration

declarationEvaluator :: Declaration -> Computation Declaration
declarationEvaluator declaration@(DLet anIdentifier aType anExpression) = do
    check anExpression aType
    addValue anIdentifier anExpression
    return declaration

check :: Expr -> Type -> Computation ()
check _ (TypeId (Ident typeIdentifier)) =
    if typeIdentifier == "Int" then return () else fail $ "Type mismatch - expected Int, got: " ++ typeIdentifier

addValue :: Ident -> Expr -> Computation ()
addValue identifier expression = do
    bindings <- get
    put $ insert identifier (IntV expression) bindings
