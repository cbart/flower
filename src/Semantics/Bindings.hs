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


type Bindings = Map Ident (Expr, Type, [Poly])

lookupType :: Ident -> Bindings -> Maybe Type
lookupType i b = onlyType <$> lookup i b

onlyType :: (Expr, Type, [Poly]) -> Type
onlyType (_, t, _) = t
