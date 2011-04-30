module Syntax.PrettyPrinter.Token where

import Syntax.Token
import Syntax.Language (keywordMapping, symbolMapping)

instance Show Token where
    show (TokSymbol symbol) = show symbol
    show (TokConst const) = show const
    show (TokIdent ident) = ident
    show (TokKeyword keyword) = show keyword

instance Show Symbol where
    show = showByMapping "symbol" symbolMapping

instance Show Const where
    show (ConstInt integer) = show integer
    show (ConstFloat double) = show double
    show (ConstBool bool) = if bool then "true" else "false"
    show (ConstChar char) = show char
    show (ConstString string) = string

instance Show Keyword where
    show = showByMapping "keyword" keywordMapping

showByMapping :: Eq a => String -> [(String, a)] -> a -> String
showByMapping typeName mapping name =
    case lookup name $ map swap mapping of
        Just string -> concat [typeName, " \"", string, "\""]
        Nothing -> concat ["unknown ", typeName]
    where
        swap :: (b, c) -> (c, b)
        swap (b, c) = (c, b)
