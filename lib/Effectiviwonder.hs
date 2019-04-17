{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, KindSignatures, 
             DataKinds, PolyKinds, TypeOperators, 
             AllowAmbiguousTypes #-}
module Effectiviwonder () where 

import Data.RBR
import Data.Kind
import GHC.TypeLits

class Capable (env :: Type) (m :: Type -> Type) (name :: k) (c :: (Type -> Type) -> Type) where
    getCapability :: env -> c m

type family MultiCapable (env :: Type) (m :: Type -> Type) (cs :: [ (k, (Type -> Type) -> Type) ]) :: Constraint where
    MultiCapable _   _ '[]       = ()
    MultiCapable env m ( '(name, c) ': xs ) = (Capable env m name c, MultiCapable env m xs)


