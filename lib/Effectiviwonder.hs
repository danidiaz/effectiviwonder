{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, KindSignatures, 
             DataKinds, PolyKinds, TypeOperators, 
             ScopedTypeVariables,
             FlexibleInstances,
             AllowAmbiguousTypes,
             TypeApplications #-}
module Effectiviwonder (
        Capable (..)
    ,   MultiCapable (..) 
    ,   Capabilities (..)
) where 

import Data.RBR
import Data.Kind
import GHC.TypeLits

class Capable (env :: Type) (m :: Type -> Type) (name :: k) (c :: (Type -> Type) -> Type) where
    getCapability :: env -> c m

type family MultiCapable (env :: Type) (m :: Type -> Type) (cs :: [ (k, (Type -> Type) -> Type) ]) :: Constraint where
    MultiCapable _   _ '[]                  = ()
    MultiCapable env m ( '(name, c) ': xs ) = (Capable env m name c, MultiCapable env m xs)

-- There's no reqirement to use a Capabilities as the environment, but is is convenient
newtype Capabilities (t :: Map Symbol Type) = Capabilities (Record I t)

instance (Key name t, Value name t ~ c m) => Capable (Capabilities t) m name c where
    getCapability (Capabilities r) = getFieldI @name r

