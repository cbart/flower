module Semantics.Type.Primitives where


import Syntax.Token (Ident)
import Syntax.Abstract


int :: Type
int = simple "Int"

char :: Type
char = simple "Char"

bool :: Type
bool = simple "Bool"

float :: Type
float = simple "Float"

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
