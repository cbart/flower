module Main where

import IO ( stdin, hGetContents )
import System ( getArgs, getProgName )

import Syntax.LexFlower
import Syntax.ParFlower
import Syntax.SkelFlower
import Syntax.PrintFlower
import Syntax.AbsFlower
import Syntax.ErrM

import Semantics.EvalFlower

type Lexer = [Char] -> [Token]
type Parser = [Token] -> Err Program
type Evaluator = Program -> Err Program

flowerLexer = myLexer
flowerParser = pProgram
flowerEvaluator = eval

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> hGetContents stdin >>= run
        fileName -> mapM_ runFile fileName

runFile :: FilePath -> IO ()
runFile filePath = do
    putStrLn filePath
    fileContent <- readFile filePath
    run fileContent

run :: String -> IO ()
run sourceCode =
    case parseFlower sourceCode of
        Ok abstractSyntax -> showAbstractSyntax abstractSyntax
        Bad errorMessage -> showError errorMessage
    where
        parseFlower = parse flowerLexer flowerParser flowerEvaluator

parse :: Lexer -> Parser -> Evaluator -> [Char] -> Err Program
parse lexer parser evaluator sourceCode = do
    tokens <- return $ lexer sourceCode
    abstractSyntax <- parser tokens
    evaluator abstractSyntax

showAbstractSyntax :: (Print a, Show a) => a -> IO ()
showAbstractSyntax abstractSyntax = do
    putStrLn "\nParse Successful!\n"
    showTree abstractSyntax

showError :: String -> IO ()
showError errorMessage = do
    putStrLn "\nParse Failed!\n"
    putStrLn errorMessage

showTree :: (Show a, Print a) => a -> IO ()
showTree tree = do
    putStrLn $ "\n[Abstract Syntax]\n\n" ++ show tree
    putStrLn $ "\n[Linearized tree]\n\n" ++ printTree tree