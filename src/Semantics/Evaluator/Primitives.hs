module Semantics.Evaluator.Primitives
( Evaluator
, runEvaluator
, get
, put
, bind
) where


import Data.Either.Unwrap
import Control.Monad.State
import Syntax.Token (Ident)
import Syntax.Abstract
import Semantics.Bindings
import Semantics.Error.Primitives


type Evaluator a = StateT Bindings (Either EvaluationError) a

runEvaluator :: Evaluator a -> Bindings -> Either EvaluationError a
runEvaluator = evalStateT

bind :: Ident -> Expr -> Type -> [Poly] -> Evaluator ()
bind i e t b = get >>= put . insert i (e, t, b)
