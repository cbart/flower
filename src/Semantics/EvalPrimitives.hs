module Semantics.EvalPrimitives where


import Data.Char
import Data.Map
import Control.Monad.Identity
import Syntax.Token
import Semantics.Abstract
import Semantics.Runner


implementation :: Map Ident Eval -> Map Ident Eval
implementation =
    intImplementation .
    floatImplementation .
    boolImplementation .
    charImplementation .
    maybeImplementation .
    streamImplementation .
    pairImplementation

intImplementation :: Map Ident Eval -> Map Ident Eval
intImplementation =
    insert "sum" (liftN2 (+) (+)) .
    insert "sub" (liftN2 (-) (-)) .
    insert "mul" (liftN2 (*) (*)) .
    insert "quot" (evalFun2 iQuot) .
    insert "mod" (evalFun2 iMod) .
    insert "neg" (evalFun $ EvalInt . negate . runInt) .
    insert "eq" (evalFun2 iEq) .
    insert "leq" (evalFun2 iLeq) .
    insert "float" (evalFun $ EvalFloat . read . show . runInt)

iQuot _ (EvalInt 0) = mNone
iQuot i1 i2 = mSome $ EvalInt $ runInt i1 `div` runInt i2

iMod _ (EvalInt 0) = mNone
iMod i1 i2 = mSome $ EvalInt $ runInt i1 `mod` runInt i2

iEq i1 i2 = EvalBool $ runInt i1 == runInt i2

iLeq i1 i2 = EvalBool $ runInt i1 <= runInt i2

floatImplementation :: Map Ident Eval -> Map Ident Eval
floatImplementation =
    insert "sumf" (liftN2 (+) (+)) .
    insert "subf" (liftN2 (-) (-)) .
    insert "mulf" (liftN2 (*) (*)) .
    insert "divf" (evalFun2 fDiv) .
    insert "negf" (evalFun $ EvalFloat . negate . runFloat) .
    insert "eqf" (evalFun2 fEq) .
    insert "leqf" (evalFun2 fLeq) .
    insert "floor" (evalFun $ EvalInt . floor . runFloat) .
    insert "ceil" (evalFun $ EvalInt . ceiling . runFloat)

fDiv :: Eval -> Eval -> Eval
fDiv _ (EvalFloat 0.0) = mNone
fDiv f1 f2 = mSome $ EvalFloat $ (/) (runFloat f1) (runFloat f2)

fEq :: Eval -> Eval -> Eval
fEq f1 f2 = EvalBool $ runFloat f1 == runFloat f2

fLeq :: Eval -> Eval -> Eval
fLeq f1 f2 = EvalBool $ runFloat f1 <= runFloat f2

boolImplementation :: Map Ident Eval -> Map Ident Eval
boolImplementation =
    insert "and" (evalFun2 bAnd) .
    insert "not" (evalFun $ EvalBool . not . runBool)

bAnd :: Eval -> Eval -> Eval
bAnd b1 b2 = EvalBool $ runBool b1 && runBool b2

charImplementation :: Map Ident Eval -> Map Ident Eval
charImplementation =
    insert "upper" (evalFun $ EvalChar . toUpper . runChar) .
    insert "lower" (evalFun $ EvalChar . toLower . runChar) .
    insert "chr" (evalFun $ EvalChar . chr . fromInteger . runInt) .
    insert "ord" (evalFun $ EvalInt . toInteger . ord . runChar)

maybeImplementation :: Map Ident Eval -> Map Ident Eval
maybeImplementation =
    insert "some" (evalFun $ mSome) .
    insert "none" mNone .
    insert "maybe" (evalFun3 $ mMaybe)

mMaybe (EvalFun f) = runMaybe f

mNone = EvalMaybe Nothing

mSome = EvalMaybe . Just

streamImplementation :: Map Ident Eval -> Map Ident Eval
streamImplementation =
    insert "cons" (evalFun2 sCons) .
    insert "nil" (EvalStream []) .
    insert "head" (evalFun $ EvalMaybe . headM . runStream) .
    insert "tail" (evalFun $ EvalMaybe . (tailM . runStream >=> return . EvalStream))

headM :: [a] -> Maybe a
headM [] = Nothing
headM (h:_) = Just h

tailM :: [a] -> Maybe [a]
tailM [] = Nothing
tailM (_:t) = Just t

sCons :: Eval -> Eval -> Eval
sCons h t = EvalStream $ (:) h $ runStream t

pairImplementation :: Map Ident Eval -> Map Ident Eval
pairImplementation =
    insert "pair" (evalFun2 $ EvalPair) .
    insert "fst" (evalFun $ runFirst) .
    insert "snd" (evalFun $ runSecond)

liftN2 :: (Integer -> Integer -> Integer) -> (Double -> Double -> Double) -> Eval
liftN2 opi opf = evalFun2 $ liftN2' opi opf

liftN2' opi _ (EvalInt i1) (EvalInt i2) = EvalInt $ opi i1 i2
liftN2' _ opf (EvalFloat f1) (EvalFloat f2) = EvalFloat $ opf f1 f2

evalFun :: (Eval -> Eval) -> Eval
evalFun f = EvalFun $ f . irun

evalFun2 :: (Eval -> Eval -> Eval) -> Eval
evalFun2 f = EvalFun $ \l -> EvalFun $ \r -> f (irun l) (irun r)

evalFun3 :: (Eval -> Eval -> Eval -> Eval) -> Eval
evalFun3 f = EvalFun $ \a1 ->
    EvalFun $ \a2 ->
        EvalFun $ \a3 ->
            f (irun a1) (irun a2) (irun a3)

irun :: Eval -> Eval
irun = runIdentity . run
