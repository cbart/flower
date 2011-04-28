module Util.Monad
( or
) where


import Prelude hiding (or)
import Control.Monad


infix 3 `or`
or :: Monad m => Bool -> m () -> m ()
b `or` u = if b then return () else u
