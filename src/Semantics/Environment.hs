module Semantics.Environment
( Environment
, empty
, lookup
, lookupType
, insert
) where


import Prelude hiding (lookup)
import qualified Data.Map
import Control.Applicative ((<$>))
import Syntax.Token (Ident)
import Syntax.Abstract


data Environment = Mem Mem

type Mem = Data.Map.Map Ident (Expr, Context)

type Context = (Type, [Poly], Environment)

empty :: Environment
empty = Mem Data.Map.empty

lookup :: Ident -> Environment -> Maybe (Expr, Context)
lookup i (Mem m) = Data.Map.lookup i m

lookupType :: Ident -> Environment -> Maybe Type
lookupType i b = onlyType <$> lookup i b

onlyType :: (Expr, Context) -> Type
onlyType (_, (t, _, _)) = t

insert :: Ident -> (Expr, Context) -> Environment -> Environment
insert i ec (Mem m) = Mem $ Data.Map.insert i ec m
