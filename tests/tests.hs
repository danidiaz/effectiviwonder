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

--import Control.Monad.Reader
import Control.Monad.Trans.Reader

import Effectiviwonder
import Effectiviwonder.State

tests :: TestTree
tests = testGroup "Tests" [ 
                                testCase "modifyTwoStates" modifyTwoStatesTest
                          ]

modifyTwoStates :: (Monad m, 
                    Capable env m '[ '("foo",State Int), '("bar",State Int) ])
                => ReaderT env m (Int,Int)
modifyTwoStates = 
    do modify @"foo" succ
       modify @"bar" succ
       (,) <$> get @"foo" <*> get @"bar"

modifyTwoStatesTest :: Assertion
modifyTwoStatesTest = do
    s1 <- mkRefBackedState 1
    s2 <- mkRefBackedState 7
    let env = insertI @"foo" s1
            . insertI @"bar" s2
            $ unit
    (r1,r2) <- runReaderT modifyTwoStates (Capabilities env)
    assertEqual "r1" 2 r1
    assertEqual "r2" 8 r2

main :: IO ()
main = defaultMain tests
