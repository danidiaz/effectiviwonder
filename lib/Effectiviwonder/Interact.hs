{-# LANGUAGE FlexibleContexts, 
             ScopedTypeVariables, 
             ExplicitForAll,
             TypeApplications,
             TypeFamilies,
             AllowAmbiguousTypes #-}
module Effectiviwonder.Interact (
        Interact(..)
      , request
      , mkInteractFromMap
    ) where

import Effectiviwonder

import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Data.Map.Strict
import Control.Exception

data Interact req res m = Interact {
       _request :: req -> m res
    }

-- Notice that the name of the capability must be supplied through type applications.
--
request :: forall name env m req res. (Monad m, Capable name env, Capability name env ~ Interact req res m) => req -> ReaderT env m res
request req =
    do c <- getCapability @name <$> ask
       lift $ _request c req
--
--
mkInteractFromMap :: (Ord req, Show req) => Map req res -> Interact req res IO 
mkInteractFromMap f = 
    Interact $ \req -> case Data.Map.Strict.lookup req f of
                    Nothing -> throwIO (userError ("request not found in map " ++ show req))
                    Just res -> return res

