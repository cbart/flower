module Semantics.Abstract where


import Syntax.Token


data Eval =
    EvalFun (Eval -> Eval)
  | EvalIf (Eval -> Eval)
  | EvalApp Eval Eval
  | EvalInt Integer
  | EvalFloat Double
  | EvalBool Bool
  | EvalChar Char
  | EvalStream [Eval]
  | EvalPair Eval Eval
  | EvalMaybe (Maybe Eval)

runFun :: Eval -> Eval -> Eval
runFun (EvalFun f) = f
runFun (EvalIf f) = f

runInt :: Eval -> Integer
runInt (EvalInt i) = i

runFloat :: Eval -> Double
runFloat (EvalFloat f) = f

runBool :: Eval -> Bool
runBool (EvalBool b) = b

runChar :: Eval -> Char
runChar (EvalChar c) = c

runStream :: Eval -> [Eval]
runStream (EvalStream s) = s

runFirst :: Eval -> Eval
runFirst (EvalPair f _) = f

runSecond :: Eval -> Eval
runSecond (EvalPair _ s) = s

runMaybe :: (Eval -> Eval) -> Eval -> Eval -> Eval
runMaybe f e (EvalMaybe m) = maybe e f m
