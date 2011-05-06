module Semantics.Error where


import Syntax.Token
import Syntax.Abstract
import Syntax.PrettyPrinter.Abstract


type EvaluationError = String

typeMismatchError :: Monad m => Type -> Type -> m a
typeMismatchError expectedType inferredType =
    fail $ concat ["Type mismatch - expected: ", show expectedType, " got: ", show inferredType, "!"]

nameError :: Monad m => Ident -> m a
nameError aName = fail $ concat ["Could not find name \"", aName, "\"", "!"]

loopError :: Monad m => m a
loopError = fail $ "Usage of \"loop\" outside of \"fun\" is forbidden!"

runtimeError :: Monad m => m a
runtimeError = fail $ "RTE"
