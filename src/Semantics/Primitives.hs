module Semantics.Primitives (primitives) where


import Prelude hiding (maybe)
import Syntax.Token
import Syntax.Abstract
import Semantics.Environment
import Semantics.Type.Primitives


primitives :: Environment Expr -> Environment Expr
primitives =
    intPrimitives .
    floatPrimitives .
    boolPrimitives .
    charPrimitives .
    maybePrimitives .
    streamPrimitives .
    pairPrimitives


intPrimitives :: Environment Expr -> Environment Expr
intPrimitives =
    s "sum" (int ~> int ~> int) .
    s "sub" (int ~> int ~> int) .
    s "mul" (int ~> int ~> int) .
    s "quot" (int ~> int ~> maybe int) .
    s "mod" (int ~> int ~> maybe int) .
    s "neg" (int ~> int) .
    s "eq" (int ~> int ~> bool) .
    s "leq" (int ~> int ~> bool) .
    s "float" (int ~> float)

floatPrimitives :: Environment Expr -> Environment Expr
floatPrimitives =
    s "sumf" (float ~> float ~> float) .
    s "subf" (float ~> float ~> float) .
    s "mulf" (float ~> float ~> float) .
    s "divf" (float ~> float ~> maybe float) .
    s "negf" (float ~> float) .
    s "eqf" (float ~> float ~> bool) .
    s "leqf" (float ~> float ~> bool) .
    s "floor" (float ~> int) .
    s "ceil" (float ~> int)

boolPrimitives :: Environment Expr -> Environment Expr
boolPrimitives =
    s "and" (bool ~> bool ~> bool) .
    s "not" (bool ~> bool)

charPrimitives :: Environment Expr -> Environment Expr
charPrimitives =
    s "upper" (char ~> char) .
    s "lower" (char ~> char)

maybePrimitives :: Environment Expr -> Environment Expr
maybePrimitives =
    p "some" (a ~> maybe a) [poly a] .
    p "none" (maybe a) [poly a] .
    p "maybe" ((a ~> b) ~> b ~> maybe a ~> b) [poly a, poly b]

streamPrimitives :: Environment Expr -> Environment Expr
streamPrimitives =
    p "cons" (a ~> stream a ~> stream a) [poly a] .
    p "nil" (stream a) [poly a] .
    p "head" (stream a ~> maybe a) [poly a] .
    p "tail" (stream a ~> maybe (stream a)) [poly a]

pairPrimitives :: Environment Expr -> Environment Expr
pairPrimitives =
    p "pair" (a ~> b ~> pair a b) [poly a, poly b] .
    p "fst" (pair a b ~> a) [poly a, poly b] .
    p "snd" (pair a b ~> b) [poly a, poly b]

s :: Ident -> Type -> Environment Expr -> Environment Expr
s anIdent aType = p anIdent aType []

p :: Ident -> Type -> [Poly] -> Environment Expr -> Environment Expr
p anIdent aType aPoly = insert anIdent (Context aType aPoly empty, ExprIdent anIdent)

poly :: Type -> Poly
poly (TypeId a) = (a, KindId)
