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

import Effectiviwonder          (MultiCapable,Capable,Capability,getCapability,Capabilities(..),fixRecord,mfixRecord,fixManagedRecord)
import Effectiviwonder.State    (State,get,modify,mkRefBackedState,mkManagedRefBackedState)
import Effectiviwonder.Interact (Interact,request,mkInteractFromMap)
import Effectiviwonder.Yield    (Yield,yield,mkRefBackedYield,mkManagedRefBackedYield)

import Data.RBR (insertI,insert,unit) -- from red-black-record

import GHC.TypeLits
import Data.Proxy
import Data.Kind
import Data.Functor.Compose
import GHC.Generics (Generic)
import qualified Data.Map.Strict as M

import Data.SOP (All,Top,I(..),(:.:)(..))       -- from sop-core
import Control.Monad.Managed

import Test.Tasty
import Test.Tasty.HUnit (testCase,Assertion,assertEqual,assertBool)

import Control.Monad.Managed
import Control.Monad.Trans
import Control.Monad.Trans.Reader


tests :: TestTree
tests = testGroup "Tests" [ 
                                testCase "twoDifferentStates" twoDifferentStatesTest,
                                testCase "getTwoUsers" getTwoUsersTest,
                                testCase "getTwoUsers_mfix" getTwoUsersTest_mfix,
                                testCase "getTwoUsers_managed" getTwoUsersTest_managed
                          ]

-- Test for two capabilities with the same type
twoDifferentStates :: (Monad m, 
                       MultiCapable env m '[ '("foo",State Int), '("bar",State Int) ])
                   => ReaderT env m (Int,Int)
twoDifferentStates = 
    do modify @"foo" succ
       modify @"bar" succ
       (,) <$> get @"foo" <*> get @"bar"

twoDifferentStatesTest :: Assertion
twoDifferentStatesTest = do
    s1 <- mkRefBackedState 1
    s2 <- mkRefBackedState 7
    let env = insertI @"foo" s1
            . insertI @"bar" s2
            $ unit
    (r1,r2) <- runReaderT twoDifferentStates (Capabilities env)
    assertEqual "r1" 2 r1
    assertEqual "r2" 8 r2

--
--
--

-- This is the definition of a complex "Users" capability that relies on more
-- basic capabilities like State, Interact and Yield.
type UserId = Int

data User = User { userName :: String } deriving (Eq,Show)

data Users m = Users {
       _getUserById :: UserId -> m User
    }

-- Convenience method for ease of use in a ReaderT
getUserById :: forall name env m. (Monad m, Capable name env, Capability name env ~ Users m) => UserId -> ReaderT env m User
getUserById userId =
    do c <- getCapability @name <$> ask
       lift $ _getUserById c userId

-- The implementation of the "Users" capability.  It makes use of a
-- record-of-capabilities argument for accessing more basic capabilities.  The
-- names of the capabilities are received as type-level Symbols.
mkUsers :: forall iname yname sname m env. 
           (Monad m, MultiCapable env m '[ '(iname,Interact UserId User),
                                           '(yname,Yield String),
                                           '(sname,State Int) ])
        => env
        -> Users m
mkUsers env = Users mkGetUserById
  where
    mkGetUserById uid = flip runReaderT env $ -- the logic of the method is defined like the main program logic.
        do yield @yname "Looking for an user" 
           u <- request @iname uid
           modify @sname succ
           return u
--
-- The "main program logic".
getTwoUsers :: (Monad m, 
                MultiCapable env m '[ '("users",Users),
                                      '("y"    ,Yield String) ])
            => ReaderT env m (User,User)
getTwoUsers = 
    do u1 <- getUserById @"users" 1
       u2 <- getUserById @"users" 2
       yield @"y" "This is logged from the top-level logic." 
       return (u1,u2)

getTwoUsersTest :: Assertion
getTwoUsersTest = do
    s <- mkRefBackedState 1
    y <- mkRefBackedYield
    let mockReqs = M.fromList [(1,User "Foo"), (2,User "Bar")]
        env  = fixRecord
             -- "complex" capabilities that depend on others get them through the record parameter
             . insert @"users" (\env -> mkUsers @"i" @"y" @"s" (Capabilities env))
             -- "basic" capabilities that do not depend on others ignore the record parameter
             . insert @"i"     (\_   -> mkInteractFromMap mockReqs)
             . insert @"y"     (\_   -> y)
             . insert @"s"     (\_   -> s)
             $ unit
    (u1,u2) <- runReaderT getTwoUsers (Capabilities env)
    assertEqual "u1" (User "Foo") u1
    assertEqual "u2" (User "Bar") u2
    return ()

--
-- Like getTwoUsersTest, but moves the allocations into the record construction
getTwoUsersTest_mfix :: Assertion
getTwoUsersTest_mfix = do
    env <- mfixRecord
         -- "complex" capabilities that depend on others get them through the record parameter
         . insert @"users" (Compose $ \envx -> return $ mkUsers @"i" @"y" @"s" (Capabilities envx))
         -- "basic" capabilities that do not depend on others ignore the record parameter
         . insert @"i"     (Compose $ \_   -> do let mockReqs = M.fromList [(1,User "Foo"), 
                                                                            (2,User "Bar")]
                                                 return $ mkInteractFromMap mockReqs)
         . insert @"y"     (Compose $ \_   -> mkRefBackedYield)
         . insert @"s"     (Compose $ \_   -> mkRefBackedState 1)
         $ unit
    (u1,u2) <- runReaderT getTwoUsers (Capabilities env)
    assertEqual "u1" (User "Foo") u1
    assertEqual "u2" (User "Bar") u2
    return ()
--
--

--
-- Like getTwoUsersTest, but handles the allocations with Managed
getTwoUsersTest_managed :: Assertion
getTwoUsersTest_managed = do
    let env = fixManagedRecord
            -- "complex" capabilities that depend on others get them through the record parameter
            . insert @"users" (Comp $ pure $ \envx -> mkUsers @"i" @"y" @"s" (Capabilities envx))
            -- . insert @"users" (Comp $ pure $ \envx -> mkUsers @"i" @"y" @"s" (Capabilities envx))
            -- "basic" capabilitiesat do not depend on others ignore the record parameter
            . insert @"i"     (Comp $ pure $ \_   -> let mockReqs = M.fromList [(1,User "Foo"), 
                                                                                (2,User "Bar")]
                                                      in mkInteractFromMap mockReqs)
            . insert @"y"     (Comp $ fmap const $ mkManagedRefBackedYield)
            . insert @"s"     (Comp $ fmap const $ mkManagedRefBackedState 1)
            $ unit
    (u1,u2) <- with env $ runReaderT getTwoUsers . Capabilities
    assertEqual "u1" (User "Foo") u1
    assertEqual "u2" (User "Bar") u2
    return ()
--
--
--
main :: IO ()
main = defaultMain tests

