module Util.Error (liftE) where


import Control.Monad.Error


liftE :: MonadError e m => Either e a -> m a
liftE (Right a) = return a
liftE (Left e) = throwError e
