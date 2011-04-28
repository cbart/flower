module Semantics.Evaluator.Primitives
( Evaluator
, runEvaluator
, get
, put
, bind
, castEither
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

bind :: Ident -> Expr -> Type -> [(Ident, Kind)] -> Evaluator ()
bind anIdent anExpr aType bounds =
    get >>= put . insert anIdent (anExpr, aType, bounds)

castEither :: Either EvaluationError a -> Evaluator a
castEither e = eitherM e fail return
