{-# LANGUAGE FlexibleContexts, 
             ScopedTypeVariables, 
             ExplicitForAll,
             TypeApplications,
             TypeFamilies,
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

-- These constraints are kind of horrific :(
get :: forall name env m s. (MonadReader env m, Has name env, Stuff name env ~ State s m) => m s
get =
    do c <- getStuff @name <$> ask
       _get c

set :: forall name env m s. (MonadReader env m, Has name env, Stuff name env ~ State s m) => s -> m ()
set s = 
    do c <- getStuff @name <$> ask
       _set c s

modify :: forall name env m s. (MonadReader env m, Has name env, Stuff name env ~ State s m) => (s -> s) -> m ()
modify f = 
    do c <- getStuff @name <$> ask
       _modify c f

-- implementations 
mkRefBackedState :: MonadIO m => s -> IO (State s m)
mkRefBackedState s =
    do ref <- liftIO $ newIORef s
       return (State (liftIO $ readIORef ref)
                     (liftIO . writeIORef ref)
                     (liftIO . modifyIORef ref))

