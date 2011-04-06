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
type Evaluation a = StateT Bindings Err a

eval = evalProgram

evalProgram :: Program -> Err Program
evalProgram program = evalStateT (programEvaluator program) baseBindings

baseBindings :: Bindings
baseBindings = empty

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
    addValue anIdentifier anExpression
    return declaration

check :: Expr -> Type -> Evaluation ()
check anExpr aType = do
    expected <- typeName aType
    actual <- deduceType anExpr
    if expected == actual then return () else typeError expected actual

typeName :: Type -> Evaluation String
typeName (TypeId (Ident aName)) = return aName

deduceType :: Expr -> Evaluation String
deduceType (ExprConst _) = return "Int"
deduceType (ExprId identifier) = boundValueType identifier

addValue :: Ident -> Expr -> Evaluation ()
addValue identifier expression = do
    bindings <- get
    put $ insert identifier (IntV expression) bindings

boundValueType :: Ident -> Evaluation String
boundValueType identifier = do
    value <- getValue identifier
    deduceType value

getValue :: Ident -> Evaluation Expr
getValue anIdentifier@(Ident aName) = do
    bindings <- get
    maybe
        (nameError aName)
        (return . omitType)
        (lookup anIdentifier bindings)

omitType :: Value -> Expr
omitType (IntV expr) = expr

typeError :: String -> String -> Evaluation a
typeError expectedType gotType =
    fail $ concat ["Type mismatch - expected: ", expectedType,
        " got: ", gotType, "!"]

nameError :: String -> Evaluation a
nameError aName =
    fail $ concat ["Could not find name \"", aName, "\"", "!"]
