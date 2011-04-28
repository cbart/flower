module Semantics.Type.Goal (infer, Condition) where


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


infer :: Expr -> Bindings -> Either EvaluationError [Condition]
infer = inferenceStep . inferenceBase

inferenceStep :: ([Task], [Condition], TypeIndex) -> Bindings -> Either EvaluationError [Condition]
inferenceStep ([], cs, _) _ = return cs
inferenceStep ((t:ts), cs, i) b = do
    (ts', cs', i') <- solveTask b t i
    inferenceStep (ts ++ ts', cs ++ cs', i') b

inferenceBase :: Expr -> ([Task], [Condition], TypeIndex)
inferenceBase e = ([(e, startEnv)], [], typeIndex0)

solveTask :: Bindings -> Task -> TypeIndex -> Either EvaluationError ([Task], [Condition], TypeIndex)
solveTask b t@(e, _) i = runGoalT (solve e) b t i

-- Solving function
solve :: Monad m => Expr -> GoalT m ()
solve (ExprFun args expr) = do
    expectedType >>= setLoop
    functionType <- expectedType
    resultType <- foldlM yieldArg functionType args
    expr <:> resultType
solve (ExprIf ifExpr thenExpr elseExpr) = do
    ifType <- newType
    ifType <=> bool
    ifExpr <:> ifType
    thenExpr <:*> expectedType
    elseExpr <:*> expectedType
solve (ExprApp funExpr argExpr) = do
    funType <- newType
    argType <- newType
    resultType <- expectedType
    funType <=> argType ~> resultType
    funExpr <:> funType
    argExpr <:> argType
solve (ExprIdent ident) = do
    expectedType <*=*> identType ident
solve (ExprConst const) = do
    expectedType <*=> constType const
solve ExprLoop = do
    expectedType <*=*> loopType

-- Given type of whole function and an identifier representing function's
-- argument returns expected type of function result.
yieldArg :: Monad m => Type -> Ident -> GoalT m Type
yieldArg functionType arg = do
    argType <- newType
    resultType <- newType
    functionType <=> argType ~> resultType
    assume (arg, argType)
    return resultType

-- Goal Transformers
type GoalT = RWST Bindings ([Task], [Condition]) (Env, TypeIndex)

runGoalT :: Monad m => GoalT m a -> Bindings -> Task -> TypeIndex -> m ([Task], [Condition], TypeIndex)
runGoalT g b (expr, env) i = do
    ((_, i'), (t', c')) <- execRWST g b (env, i)
    return (t', c', i')

-- Check the type of an expression given the environment.
type Task = (Expr, Env)

-- Yields a task of proving that `e` is of type `t`.
infixl 2 <:>
(<:>) :: Monad m => Expr -> Type -> GoalT m ()
e <:> t = do { ((as, _, l), _) <- get ; tell ([(e, (as, t, l))], []) }

infixl 2 <:*>
(<:*>) :: Monad m => Expr -> GoalT m Type -> GoalT m ()
(<:*>) e = (=<<) (e <:>)

-- Set of assumptions, expected type a expression would have
-- and optionally type of current loop expression.
type Env = ([Assumption], ExpectedType, Maybe LoopType)

startEnv :: Env
startEnv = ([], typeVar typeIndex0, Nothing)

-- Expected expression type.
type ExpectedType = Type

expectedType :: Monad m => GoalT m ExpectedType
expectedType = do { ((_, t, _), _) <- get ; return t }

-- Type of loop expression.
type LoopType = Type

setLoop :: Monad m => LoopType -> GoalT m ()
setLoop t = modify $ \((as, e, _), i) -> ((as, e, Just t), i)

loopType :: Monad m => GoalT m LoopType
loopType = do { ((_, _, l), _) <- get ; maybe loopError return l }

-- Require the two types equal.
type Condition = (Type, Type)

infixl 2 <=>
(<=>) :: Monad m => Type -> Type -> GoalT m ()
lt <=> rt = tell ([], [(lt, rt)])

infixl 2 <*=>
(<*=>) :: Monad m => GoalT m Type -> Type -> GoalT m ()
lt <*=> rt = lt >>= (<=> rt)

infixl 2 <*=*>
(<*=*>) :: Monad m => GoalT m Type -> GoalT m Type -> GoalT m ()
(<*=*>) lt = (=<<) (lt <*=>)

-- Type constructors

constType :: Const -> Type
constType (ConstInt _) = int
constType (ConstChar _) = char
constType (ConstString _) = stream char

identType :: Monad m => Ident -> GoalT m Type
identType ident = do
    ((as, _, _), _) <- get
    let assumedType = lookup ident as
    boundType <- asks $ lookupType ident
    maybe (nameError ident) return (assumedType `orElse` boundType)

newType :: Monad m => GoalT m Type
newType = liftM typeVar $ do { (e, i) <- get ; put (e, i + 1) ; return (i + 1) }

infixr 3 ~>
(~>) :: Type -> Type -> Type
(~>) = TypeFun

-- Assume that an identifier is of given type.
type Assumption = (Ident, Type)

-- Assume that value identified by `id` is of type `t`.
assume :: Monad m => Assumption -> GoalT m ()
assume a = modify $ \((as, e, l), i) -> ((a:as, e, l), i)
