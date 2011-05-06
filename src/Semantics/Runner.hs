module Semantics.Runner where


import Control.Monad
import Syntax.Token
import Semantics.Abstract
import Semantics.Error


runString :: Monad m => Eval -> String -> m String
runString mainFun = toEval >=> run . EvalApp mainFun >=> toString

run :: Monad m => Eval -> m Eval
run (EvalApp funEval argEval) = do
    funEval <- run funEval
    case funEval of
        EvalIf ifEvalT -> run $ ifEvalT argEval
        EvalFun funEvalT -> run $ funEvalT argEval
        _ -> runtimeError
run anEval = return anEval

toEval :: Monad m => String -> m Eval
toEval = return . EvalConst . ConstString

toString :: Monad m => Eval -> m String
toString (EvalConst (ConstString aString)) = return aString
