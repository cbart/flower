module Semantics.Runner where


import Control.Monad
import Syntax.Token
import Semantics.Abstract
import Semantics.Error


runString :: Monad m => Eval -> String -> m String
runString mainFun = toEval >=> run . EvalApp mainFun >=> toString

run :: Monad m => Eval -> m Eval
run (EvalApp funEval argEval) =
    run funEval >>= run . flip runFun argEval
run anEval = return anEval

toEval :: Monad m => String -> m Eval
toEval = return . EvalStream . map EvalChar

toString :: Monad m => Eval -> m String
toString = return . map runChar . runStream
