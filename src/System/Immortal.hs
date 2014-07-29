{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, FlexibleContexts #-}
-- | This module is designed to be imported qualified, e.g.
--
-- >import qualified System.Immortal as Immortal
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

-- | Immortal thread identifier (including its underlying 'ThreadId')
data Thread = Thread ThreadId (IORef Bool)

-- | Spawn a new immortal thread running the given computation.
--
-- If the computation ever finishes (either normally or due to an exception),
-- it will be restarted (in the same thread).
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

-- | Stop (kill) an immortal thread.
--
-- This is the only way to really stop an immortal thread.
stop :: Thread -> IO ()
stop (Thread pid stopRef) = do
  writeIORef stopRef True
  killThread pid

-- | Get the 'ThreadId' of the immortal thread.
--
-- The 'ThreadId' can be used to throw asynchronous exception to interrupt
-- the computation. This won't kill the thread, however — even if the
-- exception is not handled, the computation will be simply restarted.
threadId :: Thread -> ThreadId
threadId (Thread pid _) = pid
