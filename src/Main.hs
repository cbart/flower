module Main where


import Prelude hiding (lookup)
import Data.List (isSuffixOf, sort)
import Data.Map hiding (map, filter)
import System
import System.Directory
import Control.Monad
import Syntax.Token
import Syntax.Lexer
import Syntax.Parser
import Semantics.Abstract
import Semantics.Error
import Semantics.Type.Env hiding (map, filter)
import Semantics.Primitives
import Semantics.Type
import Semantics.Compiler
import Semantics.Runner (runString)
import Semantics.EvalPrimitives


main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Usage: fl fileName1 [fileName2 ...]"
        _ -> do
            source <- library args
            run (unwords args) (unlines source)

library :: [FilePath] -> IO [String]
library files = do
    let libraryPath = "./Library/"
    fileNames <- getDirectoryContents libraryPath
    let flowerFileNames = sort $ filter isFlower fileNames
    let absoluteFileNames = map (libraryPath ++) flowerFileNames
    forM (absoluteFileNames ++ files) readFile

isFlower :: FilePath -> Bool
isFlower = isSuffixOf ".fl"

run :: FilePath -> String -> IO ()
run filePath =
    runLexer filePath >=>
    runParser filePath >=>
    runTypeCheck (primitives empty) filePath >=>
    runCompiler implementation filePath >=>
    get "main" >=>
    runIO

get :: Monad m => Ident -> Map Ident Eval -> m Eval
get anIdent =
    maybe (nameError anIdent) return . lookup anIdent

runIO :: Eval -> IO ()
runIO flowerFunction =
    getContents >>= runString flowerFunction >>= putStrLn
