module Semantics.Primitives (primitives) where


import Prelude hiding (maybe)
import Syntax.Token
import Syntax.Abstract
import Semantics.Type.Env
import Semantics.Type.Primitives


primitives :: TypeEnv -> TypeEnv
primitives =
    s "sum" (int ~> int ~> int) .
    s "sub" (int ~> int ~> int) .
    s "mul" (int ~> int ~> int) .
    s "quot" (int ~> int ~> maybe int) .
    s "mod" (int ~> int ~> maybe int) .
    s "neg" (int ~> int) .
    s "eq" (int ~> int ~> bool) .
    s "leq" (int ~> int ~> bool) .
    s "float" (int ~> float) .
    s "sumf" (float ~> float ~> float) .
    s "subf" (float ~> float ~> float) .
    s "mulf" (float ~> float ~> float) .
    s "divf" (float ~> float ~> maybe float) .
    s "negf" (float ~> float) .
    s "eqf" (float ~> float ~> bool) .
    s "leqf" (float ~> float ~> bool) .
    s "floor" (float ~> int) .
    s "ceil" (float ~> int) .
    s "and" (bool ~> bool ~> bool) .
    s "not" (bool ~> bool) .
    s "upper" (char ~> char) .
    s "lower" (char ~> char) .
    s "chr" (int ~> char) .
    s "ord" (char ~> int) .
    p "some" (a ~> maybe a) [poly a] .
    p "none" (maybe a) [poly a] .
    p "maybe" ((a ~> b) ~> b ~> maybe a ~> b) [poly a, poly b] .
    p "cons" (a ~> stream a ~> stream a) [poly a] .
    p "nil" (stream a) [poly a] .
    p "head" (stream a ~> maybe a) [poly a] .
    p "tail" (stream a ~> maybe (stream a)) [poly a] .
    p "pair" (a ~> b ~> pair a b) [poly a, poly b] .
    p "fst" (pair a b ~> a) [poly a, poly b] .
    p "snd" (pair a b ~> b) [poly a, poly b]

s :: Ident -> Type -> TypeEnv -> TypeEnv
s anIdent aType = p anIdent aType []

p :: Ident -> Type -> [Poly] -> TypeEnv -> TypeEnv
p anIdent aType aPoly = insert anIdent $ Context aType aPoly empty

poly :: Type -> Poly
poly (TypeId a) = (a, KindId)
