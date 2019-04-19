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
import GHC.TypeLits
import Data.Proxy
import Data.Kind
import GHC.Generics (Generic)

import Test.Tasty
import Test.Tasty.HUnit (testCase,Assertion,assertEqual,assertBool)

import Control.Monad.Reader
import Control.Monad.Trans.Reader

import Effectiviwonder
import Effectiviwonder.State

tests :: TestTree
tests = testGroup "Tests" [ 
                                testCase "modifyTwoStates" modifyTwoStatesTest
                          ]

modifyTwoStates :: (MonadReader  env m, 
                    MultiCapable env m '[ '("foo",State Int), '("bar",State Int) ])
                => m (Int,Int)
modifyTwoStates = 
    do modify @"foo" succ
       modify @"bar" succ
       (,) <$> get @"foo" <*> get @"bar"

modifyTwoStatesTest :: Assertion
modifyTwoStatesTest = do
    --let insertI @""  
    return ()

main :: IO ()
main = defaultMain tests
