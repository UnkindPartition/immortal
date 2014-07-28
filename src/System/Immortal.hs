{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, FlexibleContexts #-}
module System.Immortal
  ( Thread
  , create
  , stop
  , threadId
  ) where

import Control.Exception.Lifted
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Concurrent.Lifted
import Data.IORef

data Thread = Thread ThreadId (IORef Bool)

create :: MonadBaseControl IO m => m () -> m Thread
create a = uninterruptibleMask $ \restore -> do
  stopRef <- liftBase $ newIORef False
  let
    go = do
      handle (\(_ :: SomeException) -> return ()) (restore a)
      stopNow <- liftBase $ readIORef stopRef
      unless stopNow go
  pid <- fork go
  return $ Thread pid stopRef

stop :: Thread -> IO ()
stop (Thread pid stopRef) = do
  writeIORef stopRef True
  killThread pid

threadId :: Thread -> ThreadId
threadId (Thread pid _) = pid
