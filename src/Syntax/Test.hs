module Syntax.Test where

import Control.Monad
import Text.Parsec.Prim
import Text.Parsec.Error
import Syntax.Abstract
import Syntax.Lexer
import Syntax.Token
import Syntax.Parser

lexFlower :: String -> Either ParseError [Token]
lexFlower = parse lexer "test"

parseFlower :: [Token] -> Either ParseError Prog
parseFlower = parse parser "test"

flowerParser = lexFlower >=> parseFlower
