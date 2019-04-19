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
             AllowAmbiguousTypes,
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

import Control.Monad.Trans
import Control.Monad.Trans.Reader

import Effectiviwonder
import Effectiviwonder.State
import Effectiviwonder.Interact
import Effectiviwonder.Yield

tests :: TestTree
tests = testGroup "Tests" [ 
                                testCase "modifyTwoStates" modifyTwoStatesTest
                          ]

modifyTwoStates :: (Monad m, 
                    MultiCapable env m '[ '("foo",State Int), '("bar",State Int) ])
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

--
--
--
type UserId = Int

data User = User { userName :: String } deriving Show

data Users m = Users {
       _getUserById :: UserId -> m User
    }

getUserById :: forall name env m. (Monad m, Capable name env, Capability name env ~ Users m) => UserId -> ReaderT env m User
getUserById userId =
    do c <- getCapability @name <$> ask
       lift $ _getUserById c userId

mkUsers :: Interact UserId User IO 
        -> Yield String IO 
        -> State Int IO
        -> Users IO
mkUsers interactor yielder stateful = Users (\uid ->
    do _yield yielder "Looking for an user" 
       u <- _request interactor uid
       _modify stateful succ
       return u)

--
--

main :: IO ()
main = defaultMain tests

