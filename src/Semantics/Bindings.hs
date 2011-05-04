module Semantics.Bindings
( Bindings
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


data Bindings = Mem Mem

type Mem = Data.Map.Map Ident (Expr, Context)

type Context = (Type, [Poly], Bindings)

empty :: Bindings
empty = Mem Data.Map.empty

lookup :: Ident -> Bindings -> Maybe (Expr, Context)
lookup i (Mem m) = Data.Map.lookup i m

lookupType :: Ident -> Bindings -> Maybe Type
lookupType i b = onlyType <$> lookup i b

onlyType :: (Expr, Context) -> Type
onlyType (_, (t, _, _)) = t

insert :: Ident -> (Expr, Context) -> Bindings -> Bindings
insert i ec (Mem m) = Mem $ Data.Map.insert i ec m
