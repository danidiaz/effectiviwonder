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
import qualified Data.Map.Strict as M
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
                                testCase "twoDifferentStates" twoDifferentStatesTest,
                                testCase "getTwoUsers" getTwoUsersTest
                          ]

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

mkUsers' :: forall iname yname sname m env. (Monad m, MultiCapable env m '[ '(iname,Interact UserId User),
                                                                            '(yname,Yield String),
                                                                            '(sname,State Int) ])
         => env
         -> Users m
mkUsers' env = Users mkGetUserById
  where
    mkGetUserById uid = flip runReaderT env $
        do yield @yname "Looking for an user" 
           u <- request @iname uid
           modify @sname succ
           return u

getTwoUsers :: (Monad m, 
                MultiCapable env m '[ '("users",Users) ])
            => ReaderT env m (User,User)
getTwoUsers = 
    do u1 <- getUserById @"users" 1
       u2 <- getUserById @"users" 2
       return (u1,u2)

getTwoUsersTest :: Assertion
getTwoUsersTest = do
    s <- mkRefBackedState 1
    let mockReqs = M.fromList [(1::Int,User "Foo"), (2::Int,User "Bar")]
        env' = insert @"users" (mkUsers' @"i" @"y" @"s")
             . insert @"i"     (\_ -> mkInteractFromMap mockReqs)
             . insert @"y"     (\_ -> Yield putStrLn)
             . insert @"s"     (\_ -> s :: State Int IO)
             $ unit
        env = fixRecord env'
    _ <- runReaderT getTwoUsers (Capabilities env)
    return ()

--
--

main :: IO ()
main = defaultMain tests

