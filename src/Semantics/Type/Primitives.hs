module Semantics.Type.Primitives where


import Syntax.Token (Ident)
import Syntax.Abstract


-- Counter used for generating free variables in type deduction
type TypeIndex = Int

-- Starting type index
typeIndex0 :: TypeIndex
typeIndex0 = 0

typeVar :: TypeIndex -> Type
typeVar i = TypeId $ concat ["$", show i]

int :: Type
int = simple "Int"

char :: Type
char = simple "Char"

bool :: Type
bool = simple "Bool"

float :: Type
float = simple "Float"

infixr 3 ~>
(~>) :: Type -> Type -> Type
(~>) = TypeFun

stream :: Type -> Type
stream = complex1 "Stream"

maybe :: Type -> Type
maybe = complex1 "Maybe"

pair :: Type -> Type -> Type
pair = complex2 "Pair"

simple :: Ident -> Type
simple = TypeId

complex1 :: Ident -> Type -> Type
complex1 = TypeApp . TypeId

complex2 :: Ident -> Type -> Type -> Type
complex2 ident t1 t2 =
    TypeApp t1 $ TypeApp t2 $ TypeId ident

-- Polymorphic

a :: Type
a = TypeId "A"

b :: Type
b = TypeId "B"
