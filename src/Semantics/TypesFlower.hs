module Semantics.TypesFlower
( Value( IntV )
) where

import Syntax.AbsFlower

data Value = IntV Expr
