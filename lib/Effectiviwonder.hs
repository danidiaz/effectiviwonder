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

class Capable (name :: Symbol) (env :: Type) where
    type Capability name env :: Type
    getCapability :: env -> Capability name env

type family MultiCapable (env :: Type) (m :: Type -> Type) (cs :: [ (Symbol, (Type -> Type) -> Type) ]) :: Constraint where
    MultiCapable _   _ '[]                  = ()
    MultiCapable env m ( '(name, c) ': xs ) = (Capable name env, Capability name env ~ c m, MultiCapable env m xs)

-- There's no reqirement to use a Capabilities as the environment, but is is convenient
newtype Capabilities (t :: Map Symbol Type) = Capabilities (Record I t)

instance Key name t => Capable name (Capabilities t) where
    type Capability name (Capabilities t) = Value name t
    getCapability (Capabilities r) = getFieldI @name r

