module Semantics.Error
( typeMismatchError
, nameError
, loopError
) where


import Syntax.Token (Ident)
import Syntax.Abstract
import Syntax.PrettyPrinter.Abstract


typeMismatchError :: Monad m => Type -> Type -> m a
typeMismatchError expectedType gotType =
    fail $ concat ["Type mismatch - expected: ", show expectedType,
        " got: ", show gotType, "!"]

nameError :: Monad m => Ident -> m a
nameError aName =
    fail $ concat ["Could not find name \"", aName, "\"", "!"]

loopError :: Monad m => m a
loopError = fail $ "Usage of \"loop\" outside of \"fun\" is forbidden!"
