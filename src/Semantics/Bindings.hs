module Semantics.Bindings
( Bindings
, empty
, lookup
, lookupType
, insert
) where


import Prelude hiding (lookup)
import qualified Data.Map as M
import Control.Applicative ((<$>))
import Syntax.Token (Ident)
import Syntax.Abstract


type Bindings = M.Map Ident (Expr, Type, [(Ident, Kind)])


empty :: Bindings
empty = M.empty

lookup :: Ident -> Bindings -> Maybe (Expr, Type, [(Ident, Kind)])
lookup = M.lookup

lookupType :: Ident -> Bindings -> Maybe Type
lookupType anIdent bindings = onlyType <$> lookup anIdent bindings

onlyType :: (Expr, Type, [(Ident, Kind)]) -> Type
onlyType (_, aType, _) = aType

insert :: Ident -> (Expr, Type, [(Ident, Kind)]) -> Bindings -> Bindings
insert = M.insert
