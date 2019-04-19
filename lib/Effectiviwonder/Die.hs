{-# LANGUAGE FlexibleContexts, 
             ScopedTypeVariables, 
             ExplicitForAll,
             TypeApplications,
             TypeFamilies,
             AllowAmbiguousTypes,
             RankNTypes
             #-}
module Effectiviwonder.Die (
        Die(..)
      , die
      , mkDieWithException
    ) where

import Effectiviwonder

import Control.Monad.IO.Class
--import Control.Monad.Reader (MonadReader(..))
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Exception

import Data.Proxy

data Die err m = Die {
       _die :: forall a. err -> m a
    }

die :: forall name env m err a. (Monad m, Capable name env, Capability name env ~ Die err m) => err -> ReaderT env m a
die err =
    do c <- getCapability @name <$> ask
       lift $ _die c err

mkDieWithException :: Die String IO 
mkDieWithException = Die (\message -> throwIO (userError message))
