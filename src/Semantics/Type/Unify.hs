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
import Semantics.Type.Infer


type UnifyT m = ReaderT Type (StateT [Condition] m)

runUnifyT :: Monad m => UnifyT m a -> Type -> [Condition] -> m a
runUnifyT = (.) evalStateT . runReaderT

-- Performs unification on given Unify monad.
unification :: Monad m => UnifyT m Type
unification = do
    conditions <- get
    case conditions of
        [] -> return $ TypeId "A"
        [c] -> ask >>= getType c
        (c:cs) -> put cs >> condition c >> unification

-- Tries to retrieve type definition from given condition.
getType :: Monad m => Condition -> Type -> UnifyT m Type
getType (leftCondType, rightCondType) aType
    | aType == leftCondType = return rightCondType
    | aType == rightCondType = return leftCondType
    | otherwise = typeMismatchError aType rightCondType

-- Unifies single condition.
condition :: Monad m => Condition -> UnifyT m ()
condition p@(leftCondType, rightCondType) = case p of
    (TypeId ('$':_), _) -> do
        baseType <- ask
        if leftCondType == baseType
            then leftCondType <=> rightCondType
            else leftCondType == rightCondType `or` leftCondType <:=> rightCondType
    (_, TypeId ('$':_)) ->
        rightCondType <=> leftCondType
    (TypeId _, TypeId _) ->
        leftCondType == rightCondType `or` typeMismatchError leftCondType rightCondType
    (TypeFun leftArgType leftResType, TypeFun rightArgType rightResType) -> do
        leftArgType <=> rightArgType
        leftResType <=> rightResType
    (TypeApp leftConstructor leftType, TypeApp rightConstructor rightType) -> do
        leftConstructor <=> rightConstructor  -- FIXME - check same kind
        leftType <=> rightType
    (_, _) -> leftCondType == rightCondType `or` typeMismatchError leftCondType rightCondType

-- Consider two types equal.
infix 4 <=>
(<=>) :: Monad m => Type -> Type -> UnifyT m ()
leftCondType <=> rightCondType = modify (++ [(leftCondType, rightCondType)])

-- Substitute `t1` for `t2` in all equations.
infix 4 <:=>
(<:=>) :: Monad m => Type -> Type -> UnifyT m ()
originalType <:=> substituteType = modify $ map $ twice (originalType `substitute` substituteType)

substitute :: Type -> Type -> Type -> Type
substitute originalType substituteType processedType =
    if originalType == processedType
        then substituteType
        else
            case processedType of
                (TypeFun funType argType) -> TypeFun (follow funType) (follow argType)
                (TypeApp typeConstructor argType) -> TypeApp typeConstructor $ follow argType
                _ -> processedType
    where follow = originalType `substitute` substituteType
