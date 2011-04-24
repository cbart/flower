module Semantics.Type
( check
) where


import Syntax.Token (Ident)
import Syntax.Abstract
import Semantics.Bindings
import Semantics.Evaluator.Primitives
import Semantics.Type.Primitives
import Semantics.Type.Goal


check :: Expr -> Type -> [(Ident, Kind)] -> Evaluator Type
check expr _ _ = do
    b <- get
    unify $ resolve b expr

resolve :: Bindings -> Expr -> [Condition]
resolve b = resolveStep b . resolveStart

resolveStart :: Expr -> ([Task], [Condition], TypeIndex)
resolveStart e = ([(e, startEnv)], [], typeIndex0)

resolveStep :: Bindings -> ([Task], [Condition], TypeIndex) -> [Condition]
resolveStep _ ([], cs, _) = cs
resolveStep b ((t:ts), cs, i) = resolveStep b (ts ++ ts', cs ++ cs', i')
    where (ts', cs', i') = solveTask b t i

unify :: [Condition] -> Evaluator Type
unify [] = return int
