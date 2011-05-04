module Semantics.Type.Teq where


import Prelude hiding (or)
import Util.Monad
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Syntax.Token
import Syntax.Abstract
import Semantics.Error


type TeqT m = ReaderT [(Ident, Kind)] (StateT [(Ident, Type)] m)

runTeqT :: Monad m => TeqT m a -> [(Ident, Kind)] -> [(Ident, Type)] -> m a
runTeqT teq = evalStateT . runReaderT teq

type InferredType = Type

type DeclaredType = Type

match :: Monad m => InferredType -> DeclaredType -> TeqT m ()
match (TypeId ('$':i)) dec = do
    m <- gets $ lookup i
    maybe (modify ((i, dec):)) (\t -> t == dec `or` typeMismatchError dec t) m
match (TypeFun t00 t01) (TypeFun t10 t11) = do
    match t00 t10
    match t01 t11
match (TypeApp t00 t01) (TypeApp t10 t11) = do
    match t00 t10
    match t01 t11
match inf dec = inf == dec `or` typeMismatchError dec inf
