module Semantics.Type.Rename (rename) where
-- DEPRECATED


import Data.Char
import Data.List
import Control.Monad
import Control.Monad.State
import Syntax.Token
import Syntax.Abstract


rename :: Monad m => Type -> m Type
rename aType = evalStateT (renamer aType) []

type RenameT = StateT RenameMap

type RenameMap = [(Ident, Ident)]

renamer :: Monad m => Type -> RenameT m Type
renamer (TypeId ('$':i)) = do { i' <- ident i ; return $ TypeId i' }
renamer t@(TypeId _) = do { return t }
renamer (TypeFun t0 t1) = do { t0' <- renamer t0 ; t1' <- renamer t1 ; return $ TypeFun t0' t1' }
renamer (TypeApp t0 t1) = do { t0' <- renamer t0 ; t1' <- renamer t1 ; return $ TypeApp t0' t1' }

ident :: Monad m => Ident -> RenameT m Ident
ident i = gets (lookup i) >>= maybe (next i) return

next :: Monad m => Ident -> RenameT m Ident
next i = do { m <- get ; let { i' = nextIdent m } ; modify ((i, i'):) ; return i' }

nextIdent :: RenameMap -> Ident
nextIdent [] = "A"
nextIdent m = nextString $ maximumBy lengthAlpha $ map snd m

nextString :: String -> String
nextString = map (\i -> chr $ i + ord 'A' - 1) . addBase 1 26 . map (\c -> ord c - ord 'A' + 1)

addBase :: Int -> Int -> [Int] -> [Int]
addBase i b is = let (m, is') = addBase' i b is in if m == 0 then is' else (m:is')
addBase' :: Int -> Int -> [Int] -> (Int, [Int])
addBase' i b [] = (i, [])
addBase' i b (ii:is) =
    let (i', is') = addBase' i b is in
    let (d, m) = (i' + ii) `divMod` b in (d, m:is')

lengthAlpha :: String -> String -> Ordering
lengthAlpha s1 s2 | l1 < l2 = LT | l1 > l2 = GT | otherwise = compare s1 s2
    where { l1 = length s1 ; l2 = length s2 }
