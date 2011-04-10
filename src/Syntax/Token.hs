module Syntax.Token
where


data Token =
    TokSymbol Symbol
  | TokConst Const
  | TokIdent Ident
  | TokKeyword Keyword
  deriving (Show, Eq)

data Symbol =
    SymColon
  | SymEqual
  | SymArrow
  | SymBracketLeft
  | SymBracketRight
  | SymAsterisk
  | SymComma
  deriving (Show, Eq)

data Const =
    ConstInt Integer
  | ConstChar Char
  | ConstString [Char]
  deriving (Show, Eq)

type Ident =
    String

data Keyword =
    KwElse
  | KwEnd
  | KwFor
  | KwFun
  | KwIf
  | KwLet
  | KwLoop
  | KwThen
  deriving (Show, Eq)
