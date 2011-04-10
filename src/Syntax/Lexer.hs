module Syntax.Lexer
( lexer
) where


import Control.Applicative ((<$>))
import Text.Parsec.Prim ((<|>))
import Text.Parsec.Combinator (sepBy1, eof)
import Syntax.Token
import Syntax.Lexer.Primitives


lexer :: Lexer u [Token]
lexer = do { tokens <- token `sepBy1` whiteSpace; eof; return tokens }

token :: Lexer u Token
token = TokSymbol <$> symbol
    <|> TokConst <$> constant
    <|> TokIdent <$> identifier
    <|> TokKeyword <$> keyword
