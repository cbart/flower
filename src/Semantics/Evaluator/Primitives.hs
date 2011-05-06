module Semantics.Evaluator.Primitives (Evaluator, runEvaluator, get, put, bind) where


import Data.Function
import Data.Either.Unwrap
import Control.Monad.State
import Syntax.Token (Ident)
import Syntax.Abstract
import Semantics.Environment
import Semantics.Error.Primitives


type Evaluator c a = StateT (Environment c) (Either EvaluationError) a

runEvaluator :: Evaluator c a -> Environment c -> Either EvaluationError a
runEvaluator = evalStateT

bind :: Ident -> Expr -> Type -> [Poly] -> Evaluator s ()
bind anIdent anExpr aType aPoly = modify $ fix . (\baseBounds thisBounds ->
    insert anIdent ((aType, aPoly, thisBounds), anExpr) baseBounds)
