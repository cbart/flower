module Semantics.Type.Primitives where


import Syntax.Token
import Syntax.Abstract


-- Counter used for generating free variables in type deduction
type TypeIndex = Int

-- Starting type index
typeIndex0 :: TypeIndex
typeIndex0 = 0

-- Creates a type variable for type inference and unification.
typeVar :: TypeIndex -> Type
typeVar i = TypeId $ concat ["$", show i]

-- Basic types

int :: Type
int = simple "Int"

char :: Type
char = simple "Char"

bool :: Type
bool = simple "Bool"

float :: Type
float = simple "Float"

-- Type constructors

infixr 3 ~>
(~>) :: Type -> Type -> Type
(~>) = TypeFun

stream :: Type -> Type
stream = complex1 "Stream"

maybe :: Type -> Type
maybe = complex1 "Maybe"

pair :: Type -> Type -> Type
pair = complex2 "Pair"

-- Polymorphic types

a :: Type
a = TypeId "A"

b :: Type
b = TypeId "B"

-- Utilities

simple :: Ident -> Type
simple = TypeId

complex1 :: Ident -> Type -> Type
complex1 = TypeApp . TypeId

complex2 :: Ident -> Type -> Type -> Type
complex2 ident =
    TypeApp . TypeApp (TypeId ident)
