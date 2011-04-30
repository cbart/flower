module Syntax.Token
where


data Token =
    TokSymbol Symbol
  | TokConst Const
  | TokIdent Ident
  | TokKeyword Keyword
  deriving (Eq)

data Symbol =
    SymColon
  | SymEqual
  | SymArrow
  | SymBracketLeft
  | SymBracketRight
  | SymAsterisk
  | SymComma
  deriving (Eq)

data Const =
    ConstInt Integer
  | ConstFloat Double
  | ConstBool Bool
  | ConstChar Char
  | ConstString String
  deriving (Eq)

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
  deriving (Eq)
