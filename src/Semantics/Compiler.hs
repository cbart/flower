module Semantics.Compiler where


import Data.Map
import Control.Monad
import Syntax.Token
import Syntax.Abstract
import Semantics.Abstract


compile :: Prog -> Map Ident Eval
compile aProg = compileProg aProg empty

compileProg :: Prog -> Map Ident Eval -> Map Ident Eval
compileProg (Prog declarations) = foldr (.) id $ Prelude.map compileDecl declarations

compileDecl :: Decl -> Map Ident Eval -> Map Ident Eval
compileDecl (Let aPoly anIdent aType anExpr) anEnv =
    let anEval = compileExpr anExpr envNew
        envNew = insert anIdent anEval anEnv
        in envNew

type EvalLoop = Eval

type MIE = Map Ident Eval

type EIM = MIE -> MIE

compileExpr :: Expr -> MIE -> Eval
compileExpr anExpr anEnv = case anExpr of
    ExprFun args exprResult ->
        let gg arg mkEval eim = EvalFun $ \eval -> mkEval (eim . insert arg eval)
            ff applyArgs = compileExpr exprResult $ applyArgs anEnv
            fun = (foldr gg ff args) (insert "loop" fun)
            in fun
    ExprIf ifExpr thenExpr elseExpr ->
        let ifEval = compileExpr ifExpr anEnv
            thenEval = compileExpr thenExpr anEnv
            elseEval = compileExpr elseExpr anEnv
            in EvalApp (EvalFun $ cond thenEval elseEval) ifEval
    ExprApp funExpr argExpr ->
        let funEval = compileExpr funExpr anEnv
            argEval = compileExpr argExpr anEnv
            in EvalApp funEval argEval
    ExprIdent anIdent ->
        anEnv ! anIdent
    ExprConst aConst ->
        EvalConst aConst

cond :: Eval -> Eval -> Eval -> Eval
cond ifTrueExpr _ (EvalConst (ConstBool True)) = ifTrueExpr
cond _ ifFalseExpr (EvalConst (ConstBool False)) = ifFalseExpr
