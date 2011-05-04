module Semantics.Bindings
( Bindings
, empty
, lookup
, lookupType
, insert
) where


import Prelude hiding (lookup)
import Data.Map
import Control.Applicative ((<$>))
import Syntax.Token (Ident)
import Syntax.Abstract


type Bindings = Map Ident (Expr, Type, [(Ident, Kind)])

lookupType :: Ident -> Bindings -> Maybe Type
lookupType i b = onlyType <$> lookup i b

onlyType :: (Expr, Type, [(Ident, Kind)]) -> Type
onlyType (_, aType, _) = aType
