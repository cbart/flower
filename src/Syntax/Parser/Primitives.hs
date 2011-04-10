{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}
module Syntax.Parser.Primitives
( Parser
, p'Symbol
, p'Const
, p'Ident
, p'Keyword
) where


import Control.Monad.Identity (Identity)
import Text.Parsec.String hiding (Parser)
import Text.Parsec.Prim (ParsecT, token, (<?>))
import Text.Parsec.Pos (SourcePos, newPos)
import Syntax.Abstract
import Syntax.Token
import Syntax.Lexer (TokenPos)


type Parser u a = ParsecT [TokenPos] u Identity a


p'Ident :: Parser u Ident
p'Ident = matchToken isIdent

p'Const :: Parser u Const
p'Const = matchToken isConst

p'Keyword :: Keyword -> Parser u Keyword
p'Keyword = matchToken . equals TokKeyword

p'Symbol :: Symbol -> Parser u Symbol
p'Symbol = matchToken . equals TokSymbol

matchToken :: (Token -> Maybe a) -> Parser u a
matchToken matcher = token show tokenPos (matcher . fst)

isIdent :: Token -> Maybe Ident
isIdent (TokIdent ident) = return ident
isIdent _ = fail "expected identifier"

isConst :: Token -> Maybe Const
isConst (TokConst const) = return const
isConst _ = fail "expected constant"

equals :: Show a => (a -> Token) -> a -> Token -> Maybe a
equals constr value token
    | token == constr value = return value
    | otherwise = fail $ "expected " ++ (show $ constr value)

tokenPos :: TokenPos -> SourcePos
tokenPos = snd
