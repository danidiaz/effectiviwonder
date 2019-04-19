{-# LANGUAGE FlexibleContexts, 
             ScopedTypeVariables, 
             ExplicitForAll,
             TypeApplications,
             TypeFamilies,
             AllowAmbiguousTypes #-}
module Effectiviwonder.Interact (
        Interact(..)
      , request
--    ,   get
--    ,   set
--    ,   modify
--    ,   mkRefBackedState
    ) where

import Effectiviwonder

import Control.Monad.IO.Class
--import Control.Monad.Reader (MonadReader(..))
import Control.Monad.Trans
import Control.Monad.Trans.Reader

import Data.Proxy

data Interact req res m = Interact {
       _request :: req -> m res
    }

request :: forall name env m req res. (Monad m, Capable name env, Capability name env ~ Interact req res m) => req -> ReaderT env m res
request req =
    do c <- getCapability @name <$> ask
       lift $ _request c req
--
-- -- These constraints are kind of horrific :(
-- get :: forall name env m s. (Monad m, Capable name env, Capability name env ~ State s m) => ReaderT env m s
-- get =
--     do c <- getCapability @name <$> ask
--        lift $ _get c
-- 
-- set :: forall name env m s. (Monad m, Capable name env, Capability name env ~ State s m) => s -> ReaderT env m ()
-- set s = 
--     do c <- getCapability @name <$> ask
--        lift $ _set c s
-- 
-- modify :: forall name env m s. (Monad m,Capable name env, Capability name env ~ State s m) => (s -> s) -> ReaderT env m ()
-- modify f = 
--     do c <- getCapability @name <$> ask
--        lift $ _modify c f
-- 
-- -- implementations 
-- mkRefBackedState :: MonadIO m => s -> IO (State s m)
-- mkRefBackedState s =
--     do ref <- newIORef s
--        return (State (liftIO $ readIORef ref)
--                      (liftIO . writeIORef ref)
--                      (liftIO . modifyIORef ref))
-- 
