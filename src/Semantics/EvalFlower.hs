module Semantics.EvalFlower
( eval
) where

{-# LANGUAGE FlexibleInstances #-}

import Prelude hiding ( lookup )
import Data.Map
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Identity

import Syntax.ErrM
import Syntax.AbsFlower

import Semantics.TypesFlower


type Bindings = Map String Value
type Computation a = ReaderT Bindings Err a

eval = evalProgram

evalProgram :: Program -> Err Program
evalProgram program = runReaderT (programEvaluator program) baseBindings

baseBindings :: Bindings
baseBindings = empty

programEvaluator :: Program -> Computation Program
programEvaluator (Program declarations) =
    liftM Program $ sequence $ do
        d <- declarations
        return $ declarationEvaluator d

declarationEvaluator :: AbstractDeclaration -> Computation AbstractDeclaration
declarationEvaluator decl = do
    return decl
