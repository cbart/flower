module Semantics.Abstract where


import Syntax.Token


data Eval =
    EvalFun (Eval -> Eval)
  | EvalIf (Eval -> Eval)
  | EvalApp Eval Eval
  | EvalConst Const
