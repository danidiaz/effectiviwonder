{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, KindSignatures, 
             DataKinds, PolyKinds, TypeOperators, 
             ScopedTypeVariables,
             FlexibleInstances,
             AllowAmbiguousTypes,
             TypeApplications,
             FlexibleContexts #-}
module Effectiviwonder (
        Capable (..) 
    ,   MultiCapable (..)
    ,   Capabilities (..)
    ,   fixRecord
) where 

import Data.RBR
import Data.SOP
import Data.SOP.NP
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

-- An easy way of getting infinite loops
fixRecord :: forall t result. (Productlike '[] t result, All Top result) => Record ((->) (Record I t)) t -> Record I t
fixRecord o = 
    let knotted = fromNP . liftA_NP (\f -> I (f knotted)) . toNP $ o
     in knotted

