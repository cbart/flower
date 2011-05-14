module Util.Error (liftE) where


import Control.Monad.Error


liftE :: Show e => Monad m => Either e a -> m a
liftE (Right a) = return a
liftE (Left e) = fail $ show e
