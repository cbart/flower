module Syntax.PrettyPrinter.Abstract where


import Syntax.PrettyPrinter.Token
import Syntax.Abstract


instance Show Type where
    show = showType 0

showType :: Int -> Type -> String
showType 0 (TypeFun t0 t1) = concat [showType 1 t0, arrow, showType 0 t1]
showType 1 (TypeApp t0 t1) = concat [showType 1 t0, " ", showType 2 t0]
showType 2 (TypeId i) = i
showType 0 t = showType 1 t
showType 1 t = showType 2 t
showType 2 t = braces $ showType 0 t

braces :: String -> String
braces s = concat ["(", s, ")"]

arrow :: String
arrow = " -> "
