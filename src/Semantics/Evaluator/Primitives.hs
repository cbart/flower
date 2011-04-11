module Semantics.Evaluator.Primitives
( Evaluator
, runEvaluator
, get
, put
, bind
) where


import qualified Control.Monad.State as S
import Syntax.Token (Ident)
import Syntax.Abstract
import Semantics.Bindings
import Semantics.Error.Primitives


type Evaluator a = S.StateT Bindings (Either EvaluationError) a


runEvaluator :: Evaluator a -> Bindings -> Either EvaluationError a
runEvaluator = S.evalStateT


get :: Evaluator Bindings
get = S.get


put :: Bindings -> Evaluator ()
put = S.put


bind :: Ident -> Expr -> Type -> [(Ident, Kind)] -> Evaluator ()
bind anIdent anExpr aType bounds =
    get >>= put . insert anIdent (anExpr, aType, bounds)
