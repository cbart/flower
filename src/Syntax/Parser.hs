module Syntax.Parser (runParser) where


import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Text.Parsec.Prim ((<|>), (<?>), runP)
import Text.Parsec.Combinator
import Util.Error
import Syntax.Abstract
import Syntax.Token
import Syntax.Lexer
import Syntax.Parser.Primitives


runParser :: Monad m => FilePath -> [TokenPos] -> m Prog
runParser filePath tokens =
    liftE $ runP (Prog <$> many1 p'Decl) () filePath tokens

p'Decl :: Parser u Decl
p'Decl = p'DeclFor <|> p'DeclLet <?> "declaration"

p'DeclFor :: Parser u Decl
p'DeclFor = do
    p'Keyword KwFor
    b <- p'DeclBounds <?> "type bounds"
    (liftM $ \(Let [] i t e) -> Let b i t e) p'DeclLet

p'DeclLet :: Parser u Decl
p'DeclLet = do
    p'Keyword KwLet
    anIdent <- p'Ident <?> "value identifier"
    p'Symbol SymColon
    aType <- p'Type <?> "value type"
    p'Symbol SymEqual
    anExpr <- p'Expr <?> "value"
    return $ Let [] anIdent aType anExpr

p'DeclBounds :: Parser u [Poly]
p'DeclBounds = p'Bound `sepBy1` p'Symbol SymComma

p'Bound :: Parser u Poly
p'Bound = do
    anIdent <- p'Ident <?> "type identifier"
    p'Symbol SymColon
    aKind <- p'Kind <?> "kind"
    return (anIdent, aKind)

p'Type :: Parser u Type
p'Type = p'TypeFun

p'TypeFun :: Parser u Type
p'TypeFun = p'TypeApp `chainr1` (o'Arrow TypeFun)

p'TypeApp :: Parser u Type
p'TypeApp = p'TypeSolid `chainl1` (o'App TypeApp)

p'TypeSolid :: Parser u Type
p'TypeSolid = parens p'TypeFun <|> p'TypeId

p'TypeId :: Parser u Type
p'TypeId = TypeId <$> p'Ident

p'Kind :: Parser u Kind
p'Kind = p'KindFun

p'KindFun :: Parser u Kind
p'KindFun = p'KindSolid `chainr1` (o'Arrow KindFun)

p'KindSolid :: Parser u Kind
p'KindSolid = parens p'Kind <|> p'KindId

p'KindId :: Parser u Kind
p'KindId = do
    p'Symbol SymAsterisk
    return KindId

p'Expr :: Parser u Expr
p'Expr = p'ExprApp <?> "expression"

p'ExprApp :: Parser u Expr
p'ExprApp = p'ExprSolid `chainl1` (o'App ExprApp)

p'ExprSolid :: Parser u Expr
p'ExprSolid = parens p'Expr
    <|> p'ExprFun
    <|> p'ExprIf
    <|> p'ExprIdent
    <|> p'ExprConst
    <|> p'ExprLoop

p'ExprFun :: Parser u Expr
p'ExprFun = do
    p'Keyword KwFun
    args <- p'Args <?> "argument identifiers"
    p'Symbol SymArrow
    expr <- p'Expr <?> "function result value"
    p'Keyword KwEnd
    return $ ExprFun args expr

p'Args :: Parser u [Ident]
p'Args = many1 p'Ident

p'ExprIf :: Parser u Expr
p'ExprIf = do
    p'Keyword KwIf
    ifExpr <- p'Expr <?> "boolean predicate"
    p'Keyword KwThen
    thenExpr <- p'Expr <?> "then-expression"
    p'Keyword KwElse
    elseExpr <- p'Expr <?> "else-expression"
    p'Keyword KwEnd
    return $ ExprIf ifExpr thenExpr elseExpr

p'ExprIdent :: Parser u Expr
p'ExprIdent = ExprIdent <$> p'Ident

p'ExprConst :: Parser u Expr
p'ExprConst = ExprConst <$> p'Const

p'ExprLoop :: Parser u Expr
p'ExprLoop = p'Keyword KwLoop >> return ExprLoop

o'Arrow :: (a -> a -> a) -> Parser u (a -> a -> a)
o'Arrow op = do
    p'Symbol SymArrow
    return op

o'App :: (a -> a -> a) -> Parser u (a -> a -> a)
o'App = return

parens :: Parser u a -> Parser u a
parens = between (p'Symbol SymBracketLeft) (p'Symbol SymBracketRight)
