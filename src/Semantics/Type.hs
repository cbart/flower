{-# LANGUAGE TypeOperators, RankNTypes #-}

module Semantics.Type
( check
) where


import Prelude hiding (or)
import Data.Either.Unwrap
import Control.Arrow
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error
import Syntax.Token (Ident)
import Syntax.Abstract
import Semantics.Error
import Semantics.Error.Primitives
import Semantics.Bindings
import Semantics.Evaluator.Primitives hiding (get, put)
import Semantics.Type.Primitives
import Semantics.Type.Goal hiding (solve, (<=>))


check :: Expr -> Type -> [(Ident, Kind)] -> Evaluator Type
check expr t _ = do
    t' <- evalType expr
    t == t' `or` typeMismatchError t t'
    return t'

evalType :: Expr -> Evaluator Type
evalType expr = do
    b <- get
    let inferred = resolve expr b >>= unify (typeVar typeIndex0)
    eitherM inferred fail return


resolve :: Expr -> Bindings -> Either EvaluationError [Condition]
resolve = resolveStep . resolveStart

resolveStart :: Expr -> ([Task], [Condition], TypeIndex)
resolveStart e = ([(e, startEnv)], [], typeIndex0)

resolveStep :: ([Task], [Condition], TypeIndex) -> Bindings -> Either EvaluationError [Condition]
resolveStep ([], cs, _) _ = return cs
resolveStep ((t:ts), cs, i) b = do
    (ts', cs', i') <- solveTask b t i
    resolveStep (ts ++ ts', cs ++ cs', i') b

unify :: Type -> [Condition] -> Either EvaluationError Type
unify = runUnify solveConditions

type Unify = ReaderT Type (StateT [Condition] (Either EvaluationError))

runUnify :: Unify a -> Type -> [Condition] -> Either EvaluationError a
runUnify u t0 cs = evalStateT (runReaderT u t0) cs

solveConditions :: Unify Type
solveConditions = get >>= solve

solve :: [Condition] -> Unify Type
solve [] = anyType
solve [c] = getType c
solve (c:cs) = put cs >> condition c >> solveConditions

anyType :: Unify Type
anyType = return $ TypeId "A"

getType :: Condition -> Unify Type
getType (t1, t2) = do
    t0 <- ask
    if t0 == t1
        then return t2
        else
            if t0 == t2
                then return t1
                else typeMismatchError t0 t2

infix 3 `or`
or :: Monad m => Bool -> m () -> m ()
b `or` u = if b then return () else u

infix 4 <=>
(<=>) :: Type -> Type -> Unify ()
t1 <=> t2 = modify (++ [(t1, t2)])

infix 4 <:=>
(<:=>) :: Type -> Type -> Unify ()
t1 <:=> t2 = modify $ map $ twice (t1 `substitute` t2)

twice :: Arrow a => a b c -> a (b, b) (c, c)
twice f = f *** f

substitute :: Type -> Type -> Type -> Type
substitute t1 t2 t = if t1 == t then t2 else
    case t of
        (TypeFun t' t'') -> TypeFun (s t') (s t'')
        (TypeApp k t') -> TypeApp k (s t')
        _ -> t
    where s = t1 `substitute` t2

condition :: Condition -> Unify ()
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
    k0 <=> k1  -- fixme
    t0 <=> t1
condition (t0, t1) = typeMismatchError t0 t1
