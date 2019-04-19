{-# LANGUAGE FlexibleContexts, AllowAmbiguousTypes #-}
module Effectiviwonder.State (
        State(..)
    ,   get
    ,   set
    ,   modify
    ,   mkRefBackedState
    ) where

import Effectiviwonder
import Data.IORef
import Control.Monad.Reader (MonadReader(..))

data State s m = State {
       _get :: m s 
    ,  _set :: s -> m ()
    ,  _modify :: (s -> s) -> m ()
    }

mkRefBackedState :: s -> IO (State s IO)
mkRefBackedState s =
    do ref <- newIORef s
       return (State (readIORef ref)
                     (writeIORef ref)
                     (modifyIORef ref))

get :: (MonadReader env m, Capable env m name (State s)) => m s
get = undefined

set :: (MonadReader env m, Capable env m name (State s)) => s -> m ()
set = undefined

modify :: (MonadReader env m, Capable env m name (State s)) => (s -> s) -> m ()
modify = undefined
