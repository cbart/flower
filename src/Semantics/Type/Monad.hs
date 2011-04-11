module Semantics.Type.Monad where


import Data.Generics.Aliases (orElse)
import Data.Foldable (foldlM)
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Syntax.Token
import Syntax.Abstract
import Semantics.Error
import Semantics.Bindings (Bindings, lookupType)
import Semantics.Type.Primitives hiding (maybe)

type TypeEvaluator a =
    ReaderT
        Bindings
        (WriterT
            [Condition]
            Identity
        )
        a
type Goal = ([Assumption], Expr, Type, Maybe Type)
type Assumption = (Ident, Type)
type Condition = (Type, Type)
type GoalEvaluator a =
    StateT
        ([Assumption], Type, Maybe Type, Int)
        (ReaderT
            Bindings
            (WriterT
                [Condition]
                Identity
            )
        )
        a


infixl 2 ====
(====) :: Type -> Type -> GoalEvaluator ()
left ==== right = tell [(left, right)]

assume :: Ident -> Type -> GoalEvaluator ()
assume anIdent aType = modify $ \(as, e, l, i) -> ((anIdent, aType) : as, e, l, i)

infixl 2 $===
($===) :: GoalEvaluator Type -> Type -> GoalEvaluator ()
leftM $=== right = do
    left <- leftM
    left ==== right

infixl 2 $==$
($==$) :: GoalEvaluator Type -> GoalEvaluator Type -> GoalEvaluator ()
leftM $==$ rightM = do
    left <- leftM
    right <- rightM
    left ==== right

infixr 3 ~>
(~>) :: Type -> Type -> Type
(~>) = TypeFun


runTypeEvaluator :: Bindings -> Expr -> [Condition]
runTypeEvaluator bindings expr =
    runIdentity $ execWriterT $ runReaderT typeEvaluator bindings
    where typeEvaluator = deduce [newGoal expr startType]

newGoal :: Expr -> Type -> Goal
newGoal expr expected = ([], expr, expected, Nothing)

deduce :: [Goal] -> TypeEvaluator ()
deduce [] = return ()
deduce (goal:goals) = do
    subgoals <- runGoal goal
    deduce (subgoals `mplus` goals)

runGoal :: Goal -> TypeEvaluator [Goal]
runGoal (assumptions, expr, expectedType, loopType) =
    runGoalEvaluator (runAction expr) assumptions expectedType loopType

runGoalEvaluator :: GoalEvaluator a -> [Assumption] -> Type -> Maybe Type -> TypeEvaluator a
runGoalEvaluator goalEvaluator assumptions expectedType loopType =
    evalStateT goalEvaluator (assumptions, expectedType, loopType, 0)

runAction :: Expr -> GoalEvaluator [Goal]
runAction (ExprFun args expr) = do
    setLoop
    resultType <- runFunction args expr
    goals [(expr, resultType)]
runAction (ExprIf ifExpr thenExpr elseExpr) = do
    ifType <- new
    ifType ==== bool
    expectedType <- expected
    goals [(ifExpr, ifType), (thenExpr, expectedType), (elseExpr, expectedType)]
runAction (ExprApp fun arg) = do
    argType <- new
    funType <- new
    resultType <- expected
    funType ==== argType ~> resultType
    goals [(arg, argType), (fun, funType)]
runAction (ExprIdent ident) = do
    identType <- getIdentType ident
    expected $=== identType
    noGoals
runAction (ExprConst const) = do
    constType <- getConstType const
    expected $=== constType
    noGoals
runAction (ExprLoop) = do
    loop $==$ expected
    noGoals

runFunction :: [Ident] -> Expr -> GoalEvaluator Type
runFunction args expr = do
    expectedType <- expected
    foldlM getResult expectedType args
    where
        getResult :: Type -> Ident -> GoalEvaluator Type
        getResult aType anIdent = do
            identType <- new
            resultType <- new
            expected $=== identType ~> resultType
            assume anIdent identType
            return resultType

loop :: GoalEvaluator Type
loop = do { l <- maybeLoop ; maybe loopError return l }

maybeLoop :: GoalEvaluator (Maybe Type)
maybeLoop = do { (_, _, l, _) <- get ; return l }

setLoop :: GoalEvaluator ()
setLoop = do
    loopType <- expected
    modify $ \(as, e, _, i) -> (as, e, Just loopType, i)

goals :: [(Expr, Type)] -> GoalEvaluator [Goal]
goals = sequence . map makeGoal
    where
        makeGoal :: (Expr, Type) -> GoalEvaluator Goal
        makeGoal (anExpr, aType) = do
            assumptions <- only anExpr
            loopType <- maybeLoop
            return (assumptions, anExpr, aType, loopType)

only :: Expr -> GoalEvaluator [Assumption]
only _ = do { (a, _, _, _) <- get ; return a }  -- FIXME

noGoals :: GoalEvaluator [Goal]
noGoals = goals []

getIdentType :: Ident -> GoalEvaluator Type
getIdentType ident = do
    assumedType <- lookupAssumedType ident
    envType <- lookupEnvType ident
    maybe (nameError ident) (return) (assumedType `orElse` envType)

lookupAssumedType :: Ident -> GoalEvaluator (Maybe Type)
lookupAssumedType ident = assumed >>= return . lookup ident

assumed :: GoalEvaluator [Assumption]
assumed = do { (a, _, _, _) <- get ; return a }

lookupEnvType :: Ident -> GoalEvaluator (Maybe Type)
lookupEnvType ident = ask >>= return . lookupType ident

getConstType :: Const -> GoalEvaluator Type
getConstType (ConstInt _) = return int
getConstType (ConstChar _) = return char
getConstType (ConstString _) = return $ stream char

expected :: GoalEvaluator Type
expected = do { (_, e, _, _) <- get; return e }

new :: GoalEvaluator Type
new = do
    (as, e, l, i) <- get
    put (as, e, l, i + 1)
    return $ typeVar (i + 1)

startType :: Type
startType = typeVar 0

typeVar :: Int -> Type
typeVar i = TypeId $ concat ["$", show i]
