module Semantics.Error
( typeError
, nameError
) where

import Semantics.EvalBase

typeError :: String -> String -> Evaluation a
typeError expectedType gotType =
    fail $ concat ["Type mismatch - expected: ", expectedType,
        " got: ", gotType, "!"]

nameError :: String -> Evaluation a
nameError aName =
    fail $ concat ["Could not find name \"", aName, "\"", "!"]
