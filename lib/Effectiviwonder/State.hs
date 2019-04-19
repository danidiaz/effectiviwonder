
module Effectiviwonder.State (
    State(..),
    mkRefBackedState
    ) where

import Effectiviwonder
import Data.IORef

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
