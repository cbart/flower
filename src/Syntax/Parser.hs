module Syntax.Parser
( parser
) where


import Control.Applicative ((<$>), (<*>))
import Text.Parsec.Prim ((<|>), (<?>))
import Text.Parsec.Combinator
import Syntax.Abstract
import Syntax.Token
import Syntax.Parser.Primitives


parser :: Parser u Prog
parser = Prog <$> many1 p'Decl

p'Decl :: Parser u Decl
p'Decl = p'DeclFor <|> p'DeclLet

p'DeclFor :: Parser u Decl
p'DeclFor = do
    p'Keyword KwFor
    bounds <- p'DeclBounds
    p'DeclLet >>= addBounds bounds

p'DeclLet :: Parser u Decl
p'DeclLet = do
    p'Keyword KwLet
    anIdent <- p'Ident
    p'Symbol SymColon
    aType <- p'Type
    p'Symbol SymEqual
    anExpr <- p'Expr
    return $ Let [] anIdent aType anExpr

p'DeclBounds :: Parser u [(Ident, Kind)]
p'DeclBounds = p'Bound `sepBy1` p'Symbol SymComma

p'Bound :: Parser u (Ident, Kind)
p'Bound = do
    anIdent <- p'Ident
    p'Symbol SymComma
    aKind <- p'Kind
    return (anIdent, aKind)

addBounds :: [(Ident, Kind)] -> Decl -> Parser u Decl
addBounds bounds (Let [] anIdent aType anExpr) =
    return $ Let bounds anIdent aType anExpr

p'Type :: Parser u Type
p'Type = p'TypeFun

p'TypeFun :: Parser u Type
p'TypeFun = p'TypeApp `chainr1` (o'Arrow TypeFun)

p'TypeApp :: Parser u Type
p'TypeApp = do
    types <- many1 p'TypeSolid
    return $ foldr1 TypeApp types

p'TypeSolid :: Parser u Type
p'TypeSolid = parens p'TypeApp <|> p'TypeId

p'TypeId :: Parser u Type
p'TypeId = TypeId <$> p'Ident

p'Kind :: Parser u Kind
p'Kind = p'KindFun <?> "kind"

p'KindFun :: Parser u Kind
p'KindFun = p'KindSolid `chainr1` (o'Arrow KindFun)

p'KindSolid :: Parser u Kind
p'KindSolid = parens p'Kind <|> p'KindId

p'KindId :: Parser u Kind
p'KindId = do
    p'Symbol SymAsterisk
    return KindId

o'Arrow :: (a -> a -> a) -> Parser u (a -> a -> a)
o'Arrow op = do
    p'Symbol SymArrow
    return op

p'Expr :: Parser u Expr
p'Expr = p'ExprApp <?> "expression"

p'ExprApp :: Parser u Expr
p'ExprApp = do
    exprs <- many1 p'ExprSolid
    return $ foldr1 ExprApp exprs

p'ExprSolid :: Parser u Expr
p'ExprSolid = parens p'ExprApp <|> p'ExprFun <|> p'ExprIf <|> p'ExprIdent <|> p'ExprConst

p'ExprFun :: Parser u Expr
p'ExprFun = do
    p'Keyword KwFun
    args <- p'Args
    p'Symbol SymArrow
    expr <- p'Expr
    p'Keyword KwEnd
    return $ ExprFun args expr

p'Args :: Parser u [Ident]
p'Args = many1 p'Ident

p'ExprIf :: Parser u Expr
p'ExprIf = do
    p'Keyword KwIf
    ifExpr <- p'Expr
    p'Keyword KwThen
    thenExpr <- p'Expr
    p'Keyword KwElse
    elseExpr <- p'Expr
    p'Keyword KwEnd
    return $ ExprIf ifExpr thenExpr elseExpr

p'ExprIdent :: Parser u Expr
p'ExprIdent = ExprIdent <$> p'Ident

p'ExprConst :: Parser u Expr
p'ExprConst = ExprConst <$> p'Const

parens :: Parser u a -> Parser u a
parens = between (p'Symbol SymBracketLeft) (p'Symbol SymBracketRight)
