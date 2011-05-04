module Semantics.Environment (Environment, empty, lookup, lookupType, insert) where


import Prelude hiding (lookup)
import qualified Data.Map
import Control.Monad
import Syntax.Token
import Syntax.Abstract


data Environment = Memory { runMemory :: Memory }

type Memory = Data.Map.Map Ident (Expr, Context)

type Context = (Type, [Poly], Environment)

empty :: Environment
empty = Memory Data.Map.empty

lookup :: Ident -> Environment -> Maybe (Expr, Context)
lookup anIdent = Data.Map.lookup anIdent . runMemory

lookupType :: Ident -> Environment -> Maybe Type
lookupType anIdent = lookup anIdent >=> (\(_, (t, _, _)) -> return t)

insert :: Ident -> (Expr, Context) -> Environment -> Environment
insert anIdent aValue = Memory . Data.Map.insert anIdent aValue . runMemory
