module Main where


import Prelude hiding (lex)
import IO (stdin, hGetContents)
import System (getArgs, getProgName)
import Control.Monad
import Text.Parsec.Error
import qualified Text.Parsec.Prim as P
import Syntax.Abstract
import Syntax.Lexer
import Syntax.Parser


main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> hGetContents stdin >>= run "stdin"
        fileNames -> mapM_ runFile fileNames

runFile :: FilePath -> IO ()
runFile filePath = do
    putStrLn filePath
    fileContent <- readFile filePath
    run filePath fileContent

run :: String -> String -> IO ()
run fileName sourceCode = do
    putStrLn ">FLOWER<"
    putStrLn "File content:"
    putStrLn $ show sourceCode
    case lex sourceCode of
        Left error -> do
            putStrLn $ show error
        Right tokens -> do
            putStrLn "\nTokens:"
            putStrLn $ show tokens
            case parse tokens of
                Left error -> putStrLn $ show error
                Right _ -> putStrLn "Parse successful!"
    where
        lex :: String -> Either ParseError [TokenPos]
        lex = P.parse lexer fileName

        parse :: [TokenPos] -> Either ParseError Prog
        parse = P.parse parser fileName
