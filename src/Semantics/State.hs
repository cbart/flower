module Semantics.State
( getValue
, bindValue
) where

import Prelude hiding ( lookup )
import Data.Map
import Control.Monad.State

import Syntax.AbsFlower

import Semantics.TypesFlower
import Semantics.EvalBase
import Semantics.Error


getValue :: Ident -> Evaluation Expr
getValue anIdentifier@(Ident aName) = do
    bindings <- get
    maybe
        (nameError aName)
        (return . omitType)
        (lookup anIdentifier bindings)

bindValue :: Ident -> Expr -> Evaluation ()
bindValue identifier expression = do
    bindings <- get
    put $ insert identifier (IntV expression) bindings

omitType :: Value -> Expr
omitType (IntV expr) = expr
