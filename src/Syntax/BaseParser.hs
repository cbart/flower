{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module Syntax.BaseParser where


import Control.Monad.Identity
import Text.Parsec.String hiding (Parser)
import qualified Text.Parsec.Prim as ParsecPrim
import qualified Text.Parsec.Pos as ParsecPos
import Syntax.Abstract
import Syntax.Token


type Parser u a = ParsecPrim.ParsecT [Token] u Identity a


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
matchToken = ParsecPrim.token show tokenPos

tokenPos :: Token -> ParsecPos.SourcePos
tokenPos _ = ParsecPos.newPos "no file" 0 0  -- FIXME
