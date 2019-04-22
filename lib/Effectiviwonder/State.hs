{-# LANGUAGE FlexibleContexts, 
             ScopedTypeVariables, 
             ExplicitForAll,
             TypeApplications,
             TypeFamilies,
             TypeOperators,
             AllowAmbiguousTypes,
             ApplicativeDo #-}
module Effectiviwonder.State (
        State(..)
    ,   get
    ,   set
    ,   modify
    ,   mkRefBackedState
    ,   mkManagedRefBackedState
    ) where

import Effectiviwonder
import Data.IORef
import Data.SOP (All,Top,I(..),(:.:)(..))       -- from sop-core
import Control.Monad.Managed
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Reader

data State s m = State {
       _get :: m s 
    ,  _set :: s -> m ()
    ,  _modify :: (s -> s) -> m ()
    }

-- Notice that the name of the capability must be supplied through type applications.
--
-- These constraints are kind of verbose :(
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

mkManagedRefBackedState :: MonadIO m => s -> Managed (State s m)
mkManagedRefBackedState s =
    do ref <- managed $ \cnt -> newIORef s >>= cnt
       pure (State (liftIO $ readIORef ref)
                   (liftIO . writeIORef ref)
                   (liftIO . modifyIORef ref))


