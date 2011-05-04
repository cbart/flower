module Semantics.Type (check) where


import Control.Monad
import Control.Monad.State
import Util.Monad
import Syntax.Token (Ident)
import Syntax.Abstract
import Semantics.Error
import Semantics.Environment hiding (lookup)
import Semantics.Evaluator.Primitives
import Semantics.Type.Primitives hiding (maybe)
import Semantics.Type.Infer
import Semantics.Type.Unify
import Semantics.Type.Rename
import Semantics.Type.Teq


check :: Expr -> Type -> [Poly] -> Evaluator Type
check anExpr decType aPoly = do
    infType <- typeOf anExpr
    infType <: (decType, aPoly)
    return decType

(<:) :: Monad m => InferredType -> (DeclaredType, [Poly]) -> m ()
infType <: (decType, aPoly) = runTeqT (match infType decType) aPoly []

typeOf :: Expr -> Evaluator Type
typeOf = infer >=> unify

infer :: Expr -> Evaluator [Condition]
infer anExpr = do { anEnv <- get ; runInferT inference anEnv anExpr }

unify :: [Condition] -> Evaluator Type
unify = runUnifyT unification $ typeVar typeIndex0
