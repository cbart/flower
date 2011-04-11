module Semantics.Type
( check
) where


import Syntax.Token (Ident)
import Syntax.Abstract
import Semantics.Evaluator.Primitives
import Semantics.Type.Monad
import Semantics.Type.Primitives


check :: Expr -> Type -> [(Ident, Kind)] -> Evaluator Type
check expr expected _ = do
    bindings <- get
    unify $ runTypeEvaluator bindings expr

unify :: [Condition] -> Evaluator Type
unify _ = return int
