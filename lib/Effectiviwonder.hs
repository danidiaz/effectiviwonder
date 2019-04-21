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
    ,   mfixRecord
) where 

import Data.RBR (Map,Key,Value,Record,getFieldI,Productlike,fromNP,toNP) -- from red-black-record
import Data.SOP (All,Top,I(..),(:.:)(..))       -- from sop-core
import Data.SOP.NP (sequence_NP,sequence'_NP)   -- from sop-core
import Data.Kind (Type,Constraint) 
import Data.Function (fix)
import Control.Monad.Fix (MonadFix,mfix)
import Control.Monad.Managed
import Data.Functor.Compose
import GHC.TypeLits

-- Basically a "Has"-like typecass that looks into a record-of-capabilities.
class Capable (name :: Symbol) (env :: Type) where
    type Capability name env :: Type
    getCapability :: env -> Capability name env

-- Shorthand for requiring multiple capabilities from a record-of-capabilities.
--
-- The capabilities in the list are not parameterized by the monad.
type family MultiCapable (env :: Type) (m :: Type -> Type) (cs :: [ (Symbol, (Type -> Type) -> Type) ]) :: Constraint where
    MultiCapable _   _ '[]                  = ()
    MultiCapable env m ( '(name, c) ': xs ) = (Capable name env, Capability name env ~ c m, MultiCapable env m xs)

-- There's no reqirement to use a Capabilities as the environment, but it is convenient
newtype Capabilities (t :: Map Symbol Type) = Capabilities (Record I t)

-- Implement Capable in terms of the Key/Value machinery of red-black-record
instance Key name t => Capable name (Capabilities t) where
    type Capability name (Capabilities t) = Value name t
    getCapability (Capabilities r) = getFieldI @name r

-- Given a record where each field is wrapped in a function that receives a "tied" version of the record, tie the record.
-- An easy way of getting infinite loops.
fixRecord :: forall t result. (Productlike '[] t result, All Top result) -- constraints required by red-black-record
          => Record ((->) (Record I t)) t 
          -> Record I t
fixRecord o = 
    let knotted = fix $ fmap fromNP . sequence_NP . toNP $ o
     in knotted

mfixRecord :: forall t result m. (Productlike '[] t result, All Top result, MonadFix m) -- constraints required by red-black-record
           => Record (Compose ((->) (Record I t)) m) t 
           -> m (Record I t)
mfixRecord o = 
    let knotted = mfix $ getCompose . fmap fromNP . sequence_NP . toNP $ o
     in knotted

fixManagedRecord :: forall t result. (Productlike '[] t result, All Top result)
                 => Record (Managed :.: (->) (Record I t)) t
                 -> Managed (Record I t)
fixManagedRecord o = 
    do np <- sequence'_NP . toNP $ o  
       let r = fromNP <$> sequence_NP np
       return $ fix r
    
