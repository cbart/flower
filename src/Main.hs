module Main where


import Prelude hiding (lex)
import IO (stdin, hGetContents)
import System (getArgs, getProgName)
import Control.Monad
import Control.Monad.Identity
import Text.Parsec.Error
import qualified Text.Parsec.Prim as P
import Syntax.Abstract
import Syntax.Lexer
import Syntax.Parser
import Semantics.Error
import Semantics.Environment
import Semantics.Primitives
import Semantics.Type
import Semantics.Compiler
import Semantics.Runner


main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> hGetContents stdin >>= Main.run "stdin"
        fileNames -> mapM_ runFile fileNames

runFile :: FilePath -> IO ()
runFile filePath = do
    putStrLn filePath
    fileContent <- readFile filePath
    Main.run filePath fileContent

run :: FilePath -> String -> IO ()
run filePath sourceCode = do
    putStrLn ">FLOWER<"
    putStrLn "File content:"
    putStrLn $ show sourceCode
    tokens <- lex filePath sourceCode
    putStrLn "\nTokens:"
    putStrLn $ show tokens
    abstractSyntax <- parse filePath tokens
    putStrLn "\nParse correct!"
    checkTypes filePath abstractSyntax
    putStrLn "\nTypes correct!"
    byteCode <- return $ compile abstractSyntax
    putStrLn $ "\nBytecode generated! " ++ (show $ size byteCode) ++ " functions"
    main <- maybe (nameError "main") return $ Semantics.Environment.lookup "main" byteCode
    putStrLn "\nMain function found!"
    putStrLn "\nRunning main on \"Dupa\""
    result <- runString main "Dupa"
    putStrLn result

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
