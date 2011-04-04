module Semantics.EvalFlower
( eval
) where

import Syntax.ErrM
import Syntax.AbsFlower

eval :: Program -> Err Program
eval = return
