module Main where


import Prelude hiding (lex)
import Data.List
import Data.Map
import IO (stdin, hGetContents)
import System (getArgs, getProgName)
import System.Directory
import Control.Monad
import Control.Monad.Identity
import Text.Parsec.Error
import qualified Text.Parsec.Prim as P
import Syntax.Token
import Syntax.Abstract
import Syntax.Lexer
import Syntax.Parser
import Semantics.Abstract
import Semantics.Error
import Semantics.Environment
import Semantics.Primitives
import Semantics.Type
import Semantics.Compiler
import Semantics.Runner
import Semantics.EvalPrimitives


main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> usage
        _ -> do
            source <- library args
            Main.run (unwords args) (unlines source)

usage :: IO ()
usage = do
    putStrLn "Usage: fl fileName1 [fileName2 ...]"

library :: [FilePath] -> IO [String]
library files = do
    let libraryPath = "./Library/"
    fileNames <- getDirectoryContents libraryPath
    let flowerFileNames = sort $ Prelude.filter isFlower fileNames
    let absoluteFileNames = Prelude.map (libraryPath ++) flowerFileNames
    forM (absoluteFileNames ++ files) readFile

isFlower :: FilePath -> Bool
isFlower = isSuffixOf ".fl"

run :: FilePath -> String -> IO ()
run filePath sourceCode = do
    tokens <- lex filePath sourceCode
    abstractSyntax <- parse filePath tokens
    checkTypes filePath abstractSyntax
    byteCode <- return $ compile abstractSyntax (implementation Data.Map.empty)
    main <- get "main" byteCode
    runInOut main

get :: Monad m => Ident -> Map Ident Eval -> m Eval
get anIdent =
    maybe (nameError anIdent) return . Semantics.Environment.lookup anIdent

runInOut :: Eval -> IO ()
runInOut flowerFunction =
    getContents >>= runString flowerFunction >>= putStrLn

lex :: Monad m => FilePath -> String -> m [TokenPos]
lex filePath sourceCode = case P.parse lexer filePath sourceCode of
    Right tokens -> return tokens
    Left lexerError -> fail $ show lexerError

parse :: Monad m => FilePath -> [TokenPos] -> m Prog
parse filePath tokens = case P.parse parser filePath tokens of
    Right abstractSyntax -> return abstractSyntax
    Left parserError -> fail $ show parserError

checkTypes :: Monad m => FilePath -> Prog -> m ()
checkTypes _ abstractSyntax = case runIdentity $ runTypeCheckT (checkProg abstractSyntax) (primitives empty) of
    Right _ -> return ()
    Left typeCheckError -> fail $ show typeCheckError
