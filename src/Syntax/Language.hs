module Syntax.Language
( flowerDef
, keywordMapping
, symbolMapping
) where


import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Syntax.Token


flowerDef :: LanguageDef st
flowerDef = LanguageDef
    {
        commentStart = "(#",
        commentEnd = "#)",
        commentLine = "#",
        nestedComments = False,
        identStart = letter,
        identLetter = alphaNum <|> char '_',
        opStart = oneOf "",
        opLetter = oneOf "",
        reservedNames = map fst keywordMapping,
        reservedOpNames = map fst symbolMapping,
        caseSensitive = True
    }

keywordMapping :: [(String, Keyword)]
keywordMapping = [
    ("else", KwElse),
    ("end", KwEnd),
    ("for", KwFor),
    ("fun", KwFun),
    ("if", KwIf),
    ("let", KwLet),
    ("loop", KwLoop),
    ("then", KwThen)
    ]

symbolMapping :: [(String, Symbol)]
symbolMapping = [
    (":", SymColon),
    ("=", SymEqual),
    ("->", SymArrow),
    ("(", SymBracketLeft),
    (")", SymBracketRight),
    ("*", SymAsterisk),
    (",", SymComma)
    ]
