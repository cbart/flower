module Semantics.Compiler where


import Data.Map
import Data.Function
import Control.Applicative
import Control.Monad
import Syntax.Token
import Syntax.Abstract
import Semantics.Abstract
import Semantics.EvalPrimitives
import Debug.Trace


compile :: Prog -> Map Ident Eval -> Map Ident Eval
compile = foldr (flip (.)) id . (<$>) compileDecl . runDecl

compileDecl :: Decl -> Map Ident Eval -> Map Ident Eval
compileDecl (Let aPoly anIdent aType anExpr) anEnv =
    let anEval = compileExpr anExpr envNew  -- LFP
        envNew = insert anIdent anEval anEnv  -- another LFP (sic!)
        in envNew

compileExpr :: Expr -> Map Ident Eval -> Eval
compileExpr anExpr anEnv = case anExpr of
    ExprFun args exprResult ->
        let processArg arg stepEval = EvalFun . (.) stepEval . (flip (.)) (insert arg) . (.)
            baseEval applyArgs = compileExpr exprResult $ applyArgs anEnv
            in fix $ foldr processArg baseEval args . insert "loop"
    ExprIf ifExpr thenExpr elseExpr ->
        EvalApp (EvalFun $ cond (follow thenExpr) (follow elseExpr)) $ irun (follow ifExpr)
    ExprApp funExpr argExpr -> EvalApp (follow funExpr) (follow argExpr)
    ExprIdent anIdent -> anEnv ! anIdent
    ExprConst (ConstInt i) -> EvalInt i
    ExprConst (ConstFloat f) -> EvalFloat f
    ExprConst (ConstBool b) -> EvalBool b
    ExprConst (ConstChar c) -> EvalChar c
    ExprConst (ConstString s) -> EvalStream $ EvalChar <$> s
    ExprLoop -> anEnv ! "loop"  -- FIXME proper runtime error // NO KIDDING - it _has_ to be there...
    where follow anotherExpr = compileExpr anotherExpr anEnv

cond :: Eval -> Eval -> Eval -> Eval
cond ifTrueExpr _ (EvalBool True) = ifTrueExpr
cond _ ifFalseExpr (EvalBool False) = ifFalseExpr
