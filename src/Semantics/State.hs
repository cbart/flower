module Semantics.State
( get
, getExpr
, getType
, bind
) where

import Prelude hiding ( lookup )
import Data.Map
import Control.Monad
import qualified Control.Monad.State as StateM

import Syntax.AbsFlower

import Semantics.EvalBase
import Semantics.Error


get :: Ident -> Evaluation (Type, Expr)
get anIdentifier@(Ident aName) = do
    bindings <- StateM.get
    maybe
        (nameError aName)
        (return)
        (lookup anIdentifier bindings)

getExpr :: Ident -> Evaluation Expr
getExpr = get >=> return . snd

getType :: Ident -> Evaluation Type
getType = get >=> return . fst

bind :: Ident -> Type -> Expr -> Evaluation ()
bind anIdent aType anExpr = do
    bindings <- StateM.get
    StateM.put $ insert anIdent (aType, anExpr) bindings
