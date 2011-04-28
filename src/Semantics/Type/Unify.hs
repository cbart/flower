{-# LANGUAGE FlexibleContexts #-}

module Semantics.Type.Unify (UnifyT, runUnifyT, unification) where


import Prelude hiding (or)
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Util.Arrow
import Util.Error
import Util.Monad
import Syntax.Abstract
import Semantics.Error
import Semantics.Error.Primitives
import Semantics.Type.Goal


type UnifyT e m = ReaderT Type (StateT [Condition] (ErrorT e m))

runUnifyT :: Error e => MonadError e m => UnifyT e m a -> Type -> [Condition] -> m a
runUnifyT u t0 = runErrorT . evalStateT (runReaderT u t0) >=> liftE

unification :: Error e => MonadError e m => UnifyT e m Type
unification = get >>= unify

unify :: Error e => MonadError e m => [Condition] -> UnifyT e m Type
unify [] = return $ TypeId "A"
unify [c] = ask >>= getType c
unify (c:cs) = put cs >> condition c >> unification

-- Tries to retrieve type definition from given condition
getType :: Error e => MonadError e m => Condition -> Type -> UnifyT e m Type
getType (t1, t2) t0
    | t0 == t1 = return t2
    | t0 == t2 = return t1
    | otherwise = typeMismatchError t0 t2

condition :: Error e => MonadError e m => Condition -> UnifyT e m ()
condition (t1@(TypeId ('$':i1)), t2) = do
    t0 <- ask
    if t1 == t0
        then t1 <=> t2
        else t1 == t2 `or` t1 <:=> t2
condition (t1, t2@(TypeId ('$':_))) = do
    t2 <=> t1
condition (t1@(TypeId _), t2@(TypeId _)) = do
    t1 == t2 `or` typeMismatchError t1 t2
condition (TypeFun t00 t01, TypeFun t10 t11) = do
    t00 <=> t10
    t01 <=> t11
condition (TypeApp k0 t0, TypeApp k1 t1) = do
    k0 <=> k1  -- FIXME - same kind
    t0 <=> t1
condition (t0, t1) = typeMismatchError t0 t1

-- Consider two types equal.
infix 4 <=>
(<=>) :: Error e => MonadError e m => Type -> Type -> UnifyT e m ()
t1 <=> t2 = modify (++ [(t1, t2)])

-- Substitute `t1` for `t2` in all equations.
infix 4 <:=>
(<:=>) :: Error e => MonadError e m => Type -> Type -> UnifyT e m ()
t1 <:=> t2 = modify $ map $ twice (t1 `substitute` t2)

substitute :: Type -> Type -> Type -> Type
substitute t1 t2 t = if t1 == t then t2 else
    case t of
        (TypeFun t' t'') -> TypeFun (s t') (s t'')
        (TypeApp k t') -> TypeApp k (s t')
        _ -> t
    where s = t1 `substitute` t2
