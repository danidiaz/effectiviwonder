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
--import Control.Monad.Reader (MonadReader(..))
import Control.Monad.Trans
import Control.Monad.Trans.Reader

import Data.Proxy

data State s m = State {
       _get :: m s 
    ,  _set :: s -> m ()
    ,  _modify :: (s -> s) -> m ()
    }

-- These constraints are kind of horrific :(
get :: forall name env m s. (Monad m, Capable name env, Capability name env ~ State s m) => ReaderT env m s
get =
    do c <- getCapability @name <$> ask
       lift $ _get c

set :: forall name env m s. (Monad m, Capable name env, Capability name env ~ State s m) => s -> ReaderT env m ()
set s = 
    do c <- getCapability @name <$> ask
       lift $ _set c s

modify :: forall name env m s. (Monad m,Capable name env, Capability name env ~ State s m) => (s -> s) -> ReaderT env m ()
modify f = 
    do c <- getCapability @name <$> ask
       lift $ _modify c f

-- implementations 
mkRefBackedState :: MonadIO m => s -> IO (State s m)
mkRefBackedState s =
    do ref <- newIORef s
       return (State (liftIO $ readIORef ref)
                     (liftIO . writeIORef ref)
                     (liftIO . modifyIORef ref))

