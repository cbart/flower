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
check expr _ _ = get >>= return . unify . resolve expr

resolve :: Expr -> Bindings -> [Condition]
resolve = resolveStep . resolveStart

resolveStart :: Expr -> ([Task], [Condition], TypeIndex)
resolveStart e = ([(e, startEnv)], [], typeIndex0)

resolveStep :: ([Task], [Condition], TypeIndex) -> Bindings -> [Condition]
resolveStep ([], cs, _) _ = cs
resolveStep ((t:ts), cs, i) b = resolveStep (ts ++ ts', cs ++ cs', i') b
    where (ts', cs', i') = solveTask b t i

unify :: [Condition] -> Type
unify _ = int  -- FIXME
