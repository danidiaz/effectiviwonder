{-# LANGUAGE FlexibleContexts, 
             ScopedTypeVariables, 
             ExplicitForAll,
             TypeApplications,
             TypeFamilies,
             AllowAmbiguousTypes #-}
module Effectiviwonder.Yield (
        Yield(..)
      , yield
      , mkRefBackedYield
    ) where

import Effectiviwonder

import Data.IORef
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Reader

data Yield y m = Yield {
       _yield :: y -> m ()
    }

-- Notice that the name of the capability must be supplied through type applications.
--
yield :: forall name env m y. (Monad m, Capable name env, Capability name env ~ Yield y m) => y -> ReaderT env m ()
yield y =
    do c <- getCapability @name <$> ask
       lift $ _yield c y

-- implementations 
mkRefBackedYield :: MonadIO m => IO (Yield y m)
mkRefBackedYield =
    do ref <- newIORef []
       return (Yield (\y -> liftIO $ modifyIORef ref (\ys -> ys ++ [y])))
