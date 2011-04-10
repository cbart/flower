module Syntax.Lexer.Primitives
where


import Control.Applicative ((<$>))
import Control.Monad.Identity (Identity)
import Text.Parsec.Char (char)
import Text.Parsec.Combinator (choice)
import Text.Parsec.Prim (ParsecT, (<|>), (<?>), try)
import qualified Text.ParserCombinators.Parsec.Token as T
import Syntax.Token
import Syntax.Language


type Lexer u a = ParsecT String u Identity a


symbol :: Lexer u Symbol
symbol = applyMapping reservedOp symbolMapping <?> "reserved symbol"

constant :: Lexer u Const
constant = ConstInt <$> constantInt <?> "literal"

constantInt :: Lexer u Integer
constantInt = (try (char '0') >> (hexadecimal <|> octal)) <|> decimal

identifier :: Lexer u Ident
identifier = T.identifier tokenLexer <?> "identifier"

keyword :: Lexer u Keyword
keyword = applyMapping reserved keywordMapping <?> "reserved keyword"

reserved :: String -> Lexer u ()
reserved = T.reserved tokenLexer

reservedOp :: String -> Lexer u ()
reservedOp = T.reservedOp tokenLexer

decimal :: Lexer u Integer
decimal = T.decimal tokenLexer

hexadecimal :: Lexer u Integer
hexadecimal = T.hexadecimal tokenLexer

octal :: Lexer u Integer
octal = T.octal tokenLexer

whiteSpace :: Lexer u ()
whiteSpace = T.whiteSpace tokenLexer

applyMapping :: (String -> Lexer u ()) -> [(String, a)] -> Lexer u a
applyMapping reservedLexer = choice . map gen
    where
        gen (aString, aMapped) = do
            reservedLexer aString
            return aMapped

tokenLexer :: T.TokenParser st
tokenLexer = T.makeTokenParser flowerDef
