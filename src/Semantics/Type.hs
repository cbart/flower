{-# LANGUAGE TypeOperators, RankNTypes #-}

module Semantics.Type (check) where


import Control.Monad
import Control.Monad.State
import Util.Monad
import Syntax.Token (Ident)
import Syntax.Abstract
import Semantics.Error
import Semantics.Bindings hiding (lookup)
import Semantics.Evaluator.Primitives
import Semantics.Type.Primitives hiding (maybe)
import Semantics.Type.Infer
import Semantics.Type.Unify
import Semantics.Type.Rename
import Semantics.Type.Teq


check :: Expr -> Type -> [Poly] -> Evaluator Type
check expr t v = do
    t' <- typeOf expr
    t' <: (t, v)
    return t

(<:) :: Monad m => InferredType -> (DeclaredType, [Poly]) -> m ()
inf <: (dec, var) = runTeqT (match inf dec) var []

typeOf :: Expr -> Evaluator Type
typeOf = infer >=> unify

infer :: Expr -> Evaluator [Condition]
infer e = do
    b <- get
    runInferT inference b (e, env) typeIndex0
    where env = ([], typeVar typeIndex0, Nothing)

unify :: [Condition] -> Evaluator Type
unify = runUnifyT unification $ TypeId "$0"
