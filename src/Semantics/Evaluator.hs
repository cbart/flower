module Semantics.Evaluator (eval) where


import Control.Monad
import Syntax.Abstract
import Semantics.Primitives
import Semantics.Error.Primitives
import Semantics.Type
import Semantics.Evaluator.Primitives


eval :: Prog -> Either EvaluationError ()
eval prog = do
    runEvaluator (e'Prog prog) primitives
    return ()

e'Prog :: Prog -> Evaluator ()
e'Prog (Prog declarations) = forM_ declarations e'Decl

e'Decl :: Decl -> Evaluator ()
e'Decl (Let bounds anIdent aType anExpr) = do
    check anExpr aType bounds
    bind anIdent anExpr aType bounds
