module Syntax.Lexer
( lexer
, TokenPos
) where


import Control.Applicative ((<$>))
import Text.Parsec.Prim ((<|>), getPosition)
import Text.Parsec.Combinator
import Text.Parsec.Pos
import Syntax.Token
import Syntax.Lexer.Primitives


type TokenPos = (Token, SourcePos)


lexer :: Lexer u [TokenPos]
lexer = do
    optional whiteSpace
    tokens <- withPos token `sepEndBy1` whiteSpace
    eof
    return tokens

withPos :: Lexer u a -> Lexer u (a, SourcePos)
withPos aLexer = do
    value <- aLexer
    position <- getPosition
    return (value, position)

token :: Lexer u Token
token = TokSymbol <$> symbol
    <|> TokConst <$> constant
    <|> TokIdent <$> identifier
    <|> TokKeyword <$> keyword
