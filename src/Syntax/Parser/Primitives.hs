{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module Syntax.Parser.Primitives
where


import Control.Monad.Identity (Identity)
import Text.Parsec.String hiding (Parser)
import Text.Parsec.Prim (ParsecT, token)
import Text.Parsec.Pos (SourcePos, newPos)
import Syntax.Abstract
import Syntax.Token


type Parser u a = ParsecT [Token] u Identity a


p'Ident :: Parser u Ident
p'Ident = do
    TokIdent anIdent <- matchToken isIdent
    return anIdent
    where
        isIdent :: Token -> Maybe Token
        isIdent token@(TokIdent _) = return token
        isIdent _ = fail "unexpected token"

p'Const :: Parser u Const
p'Const = do
    TokConst aConstant <- matchToken isConstant
    return aConstant
    where
        isConstant :: Token -> Maybe Token
        isConstant token@(TokConst _) = return token
        isConstant _ = fail "Unexpected token"

p'Keyword :: Keyword -> Parser u Keyword
p'Keyword = liftEquals TokKeyword

p'Symbol :: Symbol -> Parser u Symbol
p'Symbol = liftEquals TokSymbol

liftEquals :: (a -> Token) -> a -> Parser u a
liftEquals tokenConstructor value = do
    matchToken $ equals $ tokenConstructor value
    return value
    where
        equals aToken anotherToken
          | aToken == anotherToken = Just anotherToken
          | otherwise = Nothing

matchToken :: (Token -> Maybe Token) -> Parser u Token
matchToken = token show tokenPos

tokenPos :: Token -> SourcePos
tokenPos _ = newPos "no file" 0 0  -- FIXME
