module Syntax.PrettyPrinter.Abstract where


import Syntax.PrettyPrinter.Token
import Syntax.Abstract


instance Show Type where
    show (TypeId ident) = ident
    show (TypeFun left right) = concat [show left, " -> ", "(", show right, ")"]
    show (TypeApp left right) = concat ["(", show left, ")", " ", show right]
