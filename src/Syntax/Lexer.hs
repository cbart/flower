module Syntax.Lexer where


import Control.Applicative ((<$>))
import Control.Monad.Identity
import Text.Parsec
import Text.Parsec.Prim
import Syntax.Token
import Syntax.Language
import Syntax.BaseLexer


lexer :: Lexer u [Token]
lexer = do { tokens <- token `sepBy1` whiteSpace; eof; return tokens }

token :: Lexer u Token
token =
        TokSymbol <$> symbol
    <|> TokConst <$> constant
    <|> TokIdent <$> identifier
    <|> TokKeyword <$> keyword
