{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, KindSignatures, 
             DataKinds, PolyKinds, TypeOperators, 
             ScopedTypeVariables,
             FlexibleInstances,
             AllowAmbiguousTypes,
             TypeApplications #-}
module Effectiviwonder (
        Has (..) 
    ,   Capable (..)
    ,   Capabilities (..)
) where 

import Data.RBR
import Data.Kind
import GHC.TypeLits

class Has (name :: Symbol) (env :: Type) where
    type Stuff name env :: Type
    getStuff :: env -> Stuff name env

type family Capable (env :: Type) (m :: Type -> Type) (cs :: [ (Symbol, (Type -> Type) -> Type) ]) :: Constraint where
    Capable _   _ '[]                  = ()
    Capable env m ( '(name, c) ': xs ) = (Has name env, Stuff name env ~ c m, Capable env m xs)

-- There's no reqirement to use a Capabilities as the environment, but is is convenient
newtype Capabilities (t :: Map Symbol Type) = Capabilities (Record I t)

instance Key name t => Has name (Capabilities t) where
    type Stuff name (Capabilities t) = Value name t
    getStuff (Capabilities r) = getFieldI @name r

