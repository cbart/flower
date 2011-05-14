module Semantics.Type.Env (module Data.Map, module Semantics.Type.Env) where


import Prelude hiding (lookup)
import Data.Map
import Syntax.Token
import Syntax.Abstract


type TypeEnv = Map Ident Context

data Context = Context {
    runType :: Type,
    runPoly :: [Poly],
    runEnvironment :: TypeEnv
}
