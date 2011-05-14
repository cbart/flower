module Syntax.Lexer (runLexer, TokenPos) where


import Control.Applicative ((<$>))
import Control.Monad
import Text.Parsec.Prim ((<|>), getPosition, runP)
import Text.Parsec.Combinator
import Text.Parsec.Pos
import Text.Parsec.Error
import Util.Error
import Syntax.Token
import Syntax.Lexer.Primitives


type TokenPos = (Token, SourcePos)

runLexer :: Monad m => FilePath -> String -> m [TokenPos]
runLexer = (liftE .) . runP lexer ()

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
