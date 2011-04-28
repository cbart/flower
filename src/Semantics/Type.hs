{-# LANGUAGE TypeOperators, RankNTypes #-}

module Semantics.Type (check) where


import Prelude hiding (or)
import Control.Monad
import Control.Monad.State
import Util.Monad
import Syntax.Token (Ident)
import Syntax.Abstract
import Semantics.Error
import Semantics.Bindings
import Semantics.Evaluator.Primitives
import Semantics.Type.Primitives
import Semantics.Type.Infer
import Semantics.Type.Unify
import Semantics.Type.Rename


check :: Expr -> Type -> [(Ident, Kind)] -> Evaluator Type
check expr t _ = do
    t' <- typeOf expr
    t == t' `or` typeMismatchError t t'  -- FIXME t <= t'
    return t

typeOf :: Expr -> Evaluator Type
typeOf = infer >=> unify >=> rename

infer :: Expr -> Evaluator [Condition]
infer e = do
    b <- get
    runInferT inference b (e, env) typeIndex0
    where env = ([], typeVar typeIndex0, Nothing)

unify :: [Condition] -> Evaluator Type
unify = runUnifyT unification $ TypeId "$0"
