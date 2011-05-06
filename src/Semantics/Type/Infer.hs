module Semantics.Type.Infer (InferT, Condition, runInferT, inference) where


import Data.Foldable (foldlM)
import Data.Generics (orElse)
import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.Identity
import Control.Monad.RWS
import Syntax.Token
import Syntax.Abstract
import Semantics.Environment
import Semantics.Error
import Semantics.Error.Primitives
import Semantics.Type.Primitives hiding (maybe)


-- FIXME Errors
type InferT = RWST (Environment Expr) [Condition] ([Task], TypeIndex, Env)

runInferT :: Monad m => InferT m a -> Environment Expr -> Expr -> m [Condition]
runInferT inf anEnv = evalRWST inf anEnv . makeInferState anEnv >=> return . snd

makeInferState :: Environment Expr -> Expr -> ([Task], TypeIndex, Env)
makeInferState anEnv anExpr = ([(anExpr, baseEnv)], typeIndex0, baseEnv)
    where baseEnv = ([], typeVar typeIndex0, Nothing)

inference :: Monad m => InferT m ()
inference = do
    (tasks, typeIndex, _) <- get
    case tasks of
        [] -> return ()
        (anExpr, infEnv):otherTasks -> do
            put (otherTasks, typeIndex, infEnv)
            task anExpr
            inference

task :: Monad m => Expr -> InferT m ()
task (ExprFun args anExpr) = do
    expectedType >>= setLoop
    functionType <- expectedType
    resultType <- foldlM yieldArg functionType args
    anExpr <:> resultType
task (ExprIf ifExpr thenExpr elseExpr) = do
    ifType <- newType
    ifType <=> bool
    ifExpr <:> ifType
    thenExpr <:*> expectedType
    elseExpr <:*> expectedType
task (ExprApp funExpr argExpr) = do
    funType <- newType
    argType <- newType
    resultType <- expectedType
    funType <=> argType ~> resultType
    funExpr <:> funType
    argExpr <:> argType
task (ExprIdent anIdent) = do
    expectedType <*=*> identType anIdent
task (ExprConst aConst) = do
    expectedType <*=> constType aConst
task ExprLoop = do
    expectedType <*=*> loopType

-- Given type of whole function and an identifier representing function's
-- argument returns expected type of function result.
yieldArg :: Monad m => Type -> Ident -> InferT m Type
yieldArg functionType anArg = do
    argType <- newType
    resultType <- newType
    functionType <=> argType ~> resultType
    assume (anArg, argType)
    return resultType

-- Check the type of an expression given the environment.
type Task = (Expr, Env)

-- Yields a task of proving that `e` is of type `t`.
infixl 2 <:>
(<:>) :: Monad m => Expr -> Type -> InferT m ()
anExpr <:> aType = modify $
    \(tasks, typeIndex, infEnv@(assumptions, _, loopType)) ->
        ((anExpr, (assumptions, aType, loopType)):tasks, typeIndex, infEnv)

infixl 2 <:*>
(<:*>) :: Monad m => Expr -> InferT m Type -> InferT m ()
(<:*>) anExpr = (=<<) (anExpr <:>)

-- Set of assumptions, expected type a expression would have
-- and optionally type of current loop expression.
type Env = ([Assumption], ExpectedType, Maybe LoopType)

-- Expected expression type.
type ExpectedType = Type

expectedType :: Monad m => InferT m ExpectedType
expectedType = do { (_, _, (_, expectedType, _)) <- get ; return expectedType }

-- Type of loop expression.
type LoopType = Type

setLoop :: Monad m => LoopType -> InferT m ()
setLoop t = modify $
    \(tasks, typeIndex, (assumptions, expectedType, _)) ->
        (tasks, typeIndex, (assumptions, expectedType, Just t))

loopType :: Monad m => InferT m LoopType
loopType = do { (_, _, (_, _, loopType)) <- get ; maybe loopError return loopType }

-- Require the two types equal.
type Condition = (Type, Type)

infixl 2 <=>
(<=>) :: Monad m => Type -> Type -> InferT m ()
leftType <=> rightType = tell [(leftType, rightType)]

infixl 2 <*=>
(<*=>) :: Monad m => InferT m Type -> Type -> InferT m ()
(<*=>) leftType = (>>=) leftType . (<=>)

infixl 2 <*=*>
(<*=*>) :: Monad m => InferT m Type -> InferT m Type -> InferT m ()
(<*=*>) leftType = (=<<) (leftType <*=>)

-- Type constructors

constType :: Const -> Type
constType (ConstInt _) = int
constType (ConstFloat _) = float
constType (ConstBool _) = bool
constType (ConstChar _) = char
constType (ConstString _) = stream char

identType :: Monad m => Ident -> InferT m Type
identType anIdent = do
    (_, _, (assumptions, _, _)) <- get
    let assumedType = Prelude.lookup anIdent assumptions
    boundType <- asks $ Semantics.Environment.lookup anIdent >=> return . fst >=> return . runType
    maybe (nameError anIdent) return (assumedType `orElse` boundType)

newType :: Monad m => InferT m Type
newType = liftM typeVar $ do
    (tasks, typeIndex, infEnv) <- get
    put (tasks, typeIndex + 1, infEnv)
    return (typeIndex + 1)

infixr 3 ~>
(~>) :: Type -> Type -> Type
(~>) = TypeFun

-- Assume that an identifier is of given type.
type Assumption = (Ident, Type)

-- Assume that value identified by `id` is of type `t`.
assume :: Monad m => Assumption -> InferT m ()
assume anAssumption = modify $
    \(tasks, typeIndex, (assumptions, expectedType, loopType)) ->
        (tasks, typeIndex, (anAssumption:assumptions, expectedType, loopType))
