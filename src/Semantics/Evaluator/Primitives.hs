module Semantics.Evaluator.Primitives
( Evaluator
, runEvaluator
, get
, put
, bind
) where


import Data.Function
import Data.Either.Unwrap
import Control.Monad.State
import Syntax.Token (Ident)
import Syntax.Abstract
import Semantics.Environment
import Semantics.Error.Primitives


type Evaluator a = StateT Environment (Either EvaluationError) a

runEvaluator :: Evaluator a -> Environment -> Either EvaluationError a
runEvaluator = evalStateT

bind :: Ident -> Expr -> Type -> [Poly] -> Evaluator ()
bind i e t p = modify $ fix . (\b b' -> insert i (e, (t, p, b')) b)
