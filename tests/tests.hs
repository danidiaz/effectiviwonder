{-# LANGUAGE DataKinds,
             TypeOperators,
             TypeFamilies,
             TypeApplications,
             DeriveGeneric,
             StandaloneDeriving,
             UndecidableInstances,
             KindSignatures,
             PartialTypeSignatures,
             FlexibleContexts,
             ScopedTypeVariables
#-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Main where

import Data.RBR
import Data.SOP
import GHC.TypeLits
import Data.Proxy
import Data.Kind
import GHC.Generics (Generic)

import Test.Tasty
import Test.Tasty.HUnit (testCase,Assertion,assertEqual,assertBool)


