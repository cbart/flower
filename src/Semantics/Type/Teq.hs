module Semantics.Type.Teq where


import Prelude hiding (or)
import Util.Monad
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Syntax.Token
import Syntax.Abstract
import Semantics.Error


type TeqT m = ReaderT [Poly] (StateT [(Ident, Type)] m)

type InferredType = Type

type DeclaredType = Type

runTeqT :: Monad m => TeqT m a -> [Poly] -> [(Ident, Type)] -> m a
runTeqT = (evalStateT .) . runReaderT

-- Checks if given inferred type matches given declared type.
match :: Monad m => InferredType -> DeclaredType -> TeqT m ()
match (TypeId ('$':typeIdent)) decType = do
    prevDecType <- (gets $ lookup typeIdent)
    let bindTypeVar = modify $ (:) (typeIdent, decType)
    maybe bindTypeVar (equal decType) prevDecType
match (TypeFun leftArgType leftResType) (TypeFun rightArgType rightResType) = do
    leftArgType `match` rightArgType
    leftResType `match` rightResType
match (TypeApp leftFunType leftArgType) (TypeApp rightFunType rightArgType) = do
    leftFunType `match` rightFunType
    leftArgType `match` rightArgType
match inf@(TypeId infIdent) dec@(TypeId decIdent) = do
    polymorphic <- (asks $ lookup decIdent)
    maybe (equal dec inf) (const $ return ()) polymorphic
match inf dec = equal dec inf

-- Assure that following types _are equal_
-- if not, throw an error.
equal :: Monad m => DeclaredType -> InferredType -> m ()
equal dec inf =
    dec == inf `or` typeMismatchError dec inf
