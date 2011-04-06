module Semantics.Error
( typeMismatchError
, nameError
) where

import Syntax.AbsFlower

import Semantics.EvalBase


typeMismatchError :: Type -> Type -> Evaluation a
typeMismatchError expectedType gotType =
    fail $ concat ["Type mismatch - expected: ", typeName expectedType,
        " got: ", typeName gotType, "!"]

nameError :: String -> Evaluation a
nameError aName =
    fail $ concat ["Could not find name \"", aName, "\"", "!"]

typeName :: Type -> String
typeName (TypeId (Ident aTypeName)) = aTypeName
