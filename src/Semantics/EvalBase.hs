module Semantics.EvalBase
( Bindings
, Evaluation
, runEvaluation
) where

import Data.Map
import Control.Monad.State

import Syntax.ErrM
import Syntax.AbsFlower

type Bindings = Map Ident (Type, Expr)
type Evaluation a = StateT Bindings Err a

runEvaluation :: Evaluation a -> Bindings -> Err a
runEvaluation = evalStateT
