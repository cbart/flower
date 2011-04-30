module Semantics.Type.Infer (InferT, Condition, runInferT, inference) where


import Data.Foldable (foldlM)
import Data.Generics (orElse)
import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.Identity
import Control.Monad.RWS
import Syntax.Token
import Syntax.Abstract
import Semantics.Bindings hiding (lookup)
import Semantics.Error
import Semantics.Error.Primitives
import Semantics.Type.Primitives hiding (maybe)


-- FIXME Errors
type InferT = RWST Bindings [Condition] ([Task], TypeIndex, Env)

runInferT :: Monad m => InferT m a -> Bindings -> Task -> TypeIndex -> m [Condition]
runInferT inf b t i = do { (_, cs) <- evalRWST inf b ([t], i, snd t) ; return cs }

inference :: Monad m => InferT m ()
inference = do
    (ts, i, _) <- get
    case ts of
        [] -> return ()
        (e, env):ts -> put (ts, i, env) >> task e >> inference

task :: Monad m => Expr -> InferT m ()
task (ExprFun args expr) = do
    expectedType >>= setLoop
    functionType <- expectedType
    resultType <- foldlM yieldArg functionType args
    expr <:> resultType
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
task (ExprIdent ident) = do
    expectedType <*=*> identType ident
task (ExprConst const) = do
    expectedType <*=> constType const
task ExprLoop = do
    expectedType <*=*> loopType

-- Given type of whole function and an identifier representing function's
-- argument returns expected type of function result.
yieldArg :: Monad m => Type -> Ident -> InferT m Type
yieldArg functionType arg = do
    argType <- newType
    resultType <- newType
    functionType <=> argType ~> resultType
    assume (arg, argType)
    return resultType

-- Check the type of an expression given the environment.
type Task = (Expr, Env)

-- Yields a task of proving that `e` is of type `t`.
infixl 2 <:>
(<:>) :: Monad m => Expr -> Type -> InferT m ()
e <:> t = modify $ \(ts, i, env@(as, _, l)) -> ((e, (as, t, l)):ts, i, env)

infixl 2 <:*>
(<:*>) :: Monad m => Expr -> InferT m Type -> InferT m ()
(<:*>) e = (=<<) (e <:>)

-- Set of assumptions, expected type a expression would have
-- and optionally type of current loop expression.
type Env = ([Assumption], ExpectedType, Maybe LoopType)

-- Expected expression type.
type ExpectedType = Type

expectedType :: Monad m => InferT m ExpectedType
expectedType = do { (_, _, (_, t, _)) <- get ; return t }

-- Type of loop expression.
type LoopType = Type

setLoop :: Monad m => LoopType -> InferT m ()
setLoop t = modify $ \(ts, i, (as, e, _)) -> (ts, i, (as, e, Just t))

loopType :: Monad m => InferT m LoopType
loopType = do { (_, _, (_, _, l)) <- get ; maybe loopError return l }

-- Require the two types equal.
type Condition = (Type, Type)

infixl 2 <=>
(<=>) :: Monad m => Type -> Type -> InferT m ()
lt <=> rt = tell [(lt, rt)]

infixl 2 <*=>
(<*=>) :: Monad m => InferT m Type -> Type -> InferT m ()
lt <*=> rt = lt >>= (<=> rt)

infixl 2 <*=*>
(<*=*>) :: Monad m => InferT m Type -> InferT m Type -> InferT m ()
(<*=*>) lt = (=<<) (lt <*=>)

-- Type constructors

constType :: Const -> Type
constType (ConstInt _) = int
constType (ConstFloat _) = float
constType (ConstBool _) = bool
constType (ConstChar _) = char
constType (ConstString _) = stream char

identType :: Monad m => Ident -> InferT m Type
identType ident = do
    (_, _, (as, _, _)) <- get
    let assumedType = lookup ident as
    boundType <- asks $ lookupType ident
    maybe (nameError ident) return (assumedType `orElse` boundType)

newType :: Monad m => InferT m Type
newType = liftM typeVar $ do { (ts, i, env) <- get ; put (ts, i + 1, env) ; return (i + 1) }

infixr 3 ~>
(~>) :: Type -> Type -> Type
(~>) = TypeFun

-- Assume that an identifier is of given type.
type Assumption = (Ident, Type)

-- Assume that value identified by `id` is of type `t`.
assume :: Monad m => Assumption -> InferT m ()
assume a = modify $ \(ts, i, (as, e, l)) -> (ts, i, (a:as, e, l))
