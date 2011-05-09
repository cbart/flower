module Semantics.Type where


import Control.Monad
import Control.Monad.Error
import Control.Monad.State
import Util.Monad
import Syntax.Token (Ident)
import Syntax.Abstract
import Semantics.Error
import Semantics.Environment hiding (lookup)
import Semantics.Type.Primitives hiding (maybe)
import Semantics.Type.Infer
import Semantics.Type.Unify
import Semantics.Type.Rename
import Semantics.Type.Teq

type TypeCheckT m = StateT (Environment Expr) (ErrorT EvaluationError m)

runTypeCheckT :: Monad m => TypeCheckT m a -> Environment Expr -> m (Either EvaluationError a)
runTypeCheckT aTypeCheck anEnv = runErrorT (evalStateT aTypeCheck anEnv)

checkProg :: Monad m => Prog -> TypeCheckT m ()
checkProg (Prog declarations) = forM_ declarations checkDeclaration

checkDeclaration :: Monad m => Decl -> TypeCheckT m ()
checkDeclaration (Let aPoly anIdent aType anExpr) = do
    check anIdent anExpr aType aPoly
    modify $ fix . (\baseBounds thisBounds ->
        insert anIdent ((Context aType aPoly thisBounds), anExpr) baseBounds)

check :: Monad m => Ident -> Expr -> Type -> [Poly] -> TypeCheckT m Type
check anIdent anExpr decType aPoly = do
    infType <- typeOf anIdent decType aPoly anExpr
    infType <: (decType, aPoly)
    return decType

(<:) :: Monad m => InferredType -> (DeclaredType, [Poly]) -> m ()
infType <: (decType, aPoly) = runTeqT (match infType decType) aPoly []

typeOf :: Monad m => Ident -> Type -> [Poly] -> Expr -> TypeCheckT m Type
typeOf anIdent aType aPoly = infer anIdent aType aPoly >=> unify

infer :: Monad m => Ident -> Type -> [Poly] -> Expr -> TypeCheckT m [Condition]
infer anIdent aType aPoly anExpr = do
    anEnv <- get
    runInferT inference (insert anIdent (Context aType aPoly anEnv, ExprIdent anIdent) anEnv) anExpr

unify :: Monad m => [Condition] -> TypeCheckT m Type
unify = runUnifyT unification $ typeVar typeIndex0
