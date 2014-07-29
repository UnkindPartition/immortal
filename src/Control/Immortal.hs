{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, FlexibleContexts #-}
-- | This module is designed to be imported qualified, e.g.
--
-- >import qualified Control.Immortal as Immortal
module Control.Immortal
  ( Thread
  , create
  , createWithLabel
  , stop
  , threadId
  , onFinish
  ) where

import Control.Exception.Lifted
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Concurrent.Lifted
import Data.IORef
import GHC.Conc (labelThread)

-- | Immortal thread identifier (including its underlying 'ThreadId')
data Thread = Thread ThreadId (IORef Bool)

-- | Spawn a new immortal thread running the given computation.
--
-- If the computation ever finishes (either normally or due to an exception),
-- it will be restarted (in the same thread).
--
-- The monadic «state» (captured by the 'MonadBaseControl' instance) will
-- be preserved if the computation terminates normally, and reset when the
-- exception is thrown, so be cautious when @m@ is stateful.
-- It is completely safe, however, to instantiate @m@ with
-- something like @ReaderT conf IO@ to pass configuration to the new
-- thread.
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

-- | Like 'create', but also apply the given label to the thread
-- (using 'labelThread').
createWithLabel :: MonadBaseControl IO m => String -> m () -> m Thread
createWithLabel label a = do
  thread <- create a
  liftBase $ labelThread (threadId thread) label
  return thread

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

-- | Run a callback every time the action finishes. This can be used e.g.
-- to log exceptions or attempts to exit when such attempts are
-- not expected. Example usage:
--
-- >Immortal.create $ Immortal.onFinish print myAction
--
-- This is nothing more than a simple wrapper around 'try'.
onFinish
  :: MonadBaseControl IO m
  => (Either SomeException () -> m ())
  -> m () -> m ()
onFinish cb a = try a >>= cb
