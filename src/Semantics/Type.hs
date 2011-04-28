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
import Semantics.Type.Goal
import Semantics.Type.Unify
import Semantics.Type.Rename


check :: Expr -> Type -> [(Ident, Kind)] -> Evaluator Type
check expr t _ = do
    t' <- eval' expr
    t == t' `or` typeMismatchError t t'
    return t'

eval' :: Expr -> Evaluator Type
eval' = infer' >=> unify >=> rename

infer' :: Expr -> Evaluator [Condition]
infer' e = get >>= castEither . infer e

unify :: [Condition] -> Evaluator Type
unify = runUnifyT unification $ TypeId "$0"
