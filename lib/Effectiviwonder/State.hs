{-# LANGUAGE FlexibleContexts, 
             ScopedTypeVariables, 
             ExplicitForAll,
             TypeApplications,
             AllowAmbiguousTypes #-}
module Effectiviwonder.State (
        State(..)
    ,   get
    ,   set
    ,   modify
    ,   mkRefBackedState
    ) where

import Effectiviwonder
import Data.IORef
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader(..))

import Data.Proxy

data State s m = State {
       _get :: m s 
    ,  _set :: s -> m ()
    ,  _modify :: (s -> s) -> m ()
    }

get :: forall name env m s. (MonadReader env m, Capable env m name (State s)) => m s
get =
    do c <- getCapability @env @m @name @(State s) <$> ask
       _get c

set :: forall name env m s. (MonadReader env m, Capable env m name (State s)) => s -> m ()
set s = 
    do c <- getCapability @env @m @name @(State s) <$> ask
       _set c s

modify :: forall name env m s. (MonadReader env m, Capable env m name (State s)) => (s -> s) -> m ()
modify f = 
    do c <- getCapability @env @m @name @(State s) <$> ask
       _modify c f

-- implementations 
mkRefBackedState :: MonadIO m => s -> m (State s m)
mkRefBackedState s =
    do ref <- liftIO $ newIORef s
       return (State (liftIO $ readIORef ref)
                     (liftIO . writeIORef ref)
                     (liftIO . modifyIORef ref))

