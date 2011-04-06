module Semantics.Primitives
( primitives
) where

import Prelude hiding ( lookup )
import Data.Map

import Semantics.EvalBase

primitives :: Bindings
primitives = empty
