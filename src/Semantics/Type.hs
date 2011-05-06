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
    check anExpr aType aPoly
    modify $ fix . (\baseBounds thisBounds ->
        insert anIdent ((Context aType aPoly thisBounds), anExpr) baseBounds)

check :: Monad m => Expr -> Type -> [Poly] -> TypeCheckT m Type
check anExpr decType aPoly = do
    infType <- typeOf anExpr
    infType <: (decType, aPoly)
    return decType

(<:) :: Monad m => InferredType -> (DeclaredType, [Poly]) -> m ()
infType <: (decType, aPoly) = runTeqT (match infType decType) aPoly []

typeOf :: Monad m => Expr -> TypeCheckT m Type
typeOf = infer >=> unify

infer :: Monad m => Expr -> TypeCheckT m [Condition]
infer anExpr = do { anEnv <- get ; runInferT inference anEnv anExpr }

unify :: Monad m => [Condition] -> TypeCheckT m Type
unify = runUnifyT unification $ typeVar typeIndex0
