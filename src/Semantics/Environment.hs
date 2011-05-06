module Semantics.Environment (module Data.Map, module Semantics.Environment) where


import Prelude hiding (lookup)
import Data.Map
import Control.Monad
import Syntax.Token
import Syntax.Abstract


type Environment a = Map Ident (Context a, a)

data Context a = Context {
    runType :: Type,
    runPoly :: [Poly],
    runEnvironment :: Environment a
}
