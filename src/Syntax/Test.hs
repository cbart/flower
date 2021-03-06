module Syntax.Test
( lexFlower
, parseFlower
, testFlower
) where


import Control.Monad ((>=>))
import Text.Parsec.Prim (parse)
import Text.Parsec.Error (ParseError)
import Syntax.Token
import Syntax.PrettyPrinter.Token
import Syntax.Lexer
import Syntax.Abstract
import Syntax.Parser


lexFlower :: String -> Either ParseError [TokenPos]
lexFlower = parse lexer "test"

parseFlower :: [TokenPos] -> Either ParseError Prog
parseFlower = parse parser "test"

testFlower :: String -> Either ParseError Prog
testFlower = lexFlower >=> parseFlower
