module Semantics.EvalBase
( Bindings
, Evaluation
) where

import Data.Map
import Control.Monad.State

import Syntax.ErrM
import Syntax.AbsFlower

import Semantics.TypesFlower

type Bindings = Map Ident Value
type Evaluation a = StateT Bindings Err a
