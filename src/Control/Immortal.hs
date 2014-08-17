{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, FlexibleContexts #-}
-- | This module is designed to be imported qualified, e.g.
--
-- >import qualified Control.Immortal as Immortal
module Control.Immortal
  ( Thread
  , create
  , createWithLabel
  , mortalize
  , immortalize
  , stop
  , wait
  , waitSTM
  , threadId
  , onFinish
  ) where

import Control.Exception.Lifted
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Concurrent.Lifted
import Data.IORef
import GHC.Conc (labelThread)
import Control.Concurrent.STM

-- | Immortal thread identifier (including its underlying 'ThreadId')
data Thread = Thread ThreadId (IORef Bool) (TVar Bool)

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
create
  :: MonadBaseControl IO m
  => (Thread -> m ())
  -> m Thread
create a = uninterruptibleMask $ \restore -> do
  -- Why use uninterruptibleMask instead of just mask? We're not using any
  -- blocking operations so far, so there should be no difference. Still,
  -- better be safe than sorry. Besides, we're using operations from
  -- `MonadBaseControl` and related instances, and those could potentially
  -- (though unlikely) block.
  stopRef     <- liftBase $ newIORef  False
  finishedRef <- liftBase $ newTVarIO False
  let
    go = do
      -- construct a thread object from within the thread itself
      pid <- myThreadId
      let thread = Thread pid stopRef finishedRef

      handle (\(_ :: SomeException) -> return ()) (restore $ a thread)

      stopNow <- liftBase $ readIORef stopRef
      if stopNow then 
        liftBase $ atomically $ writeTVar finishedRef True
      else
        go
  pid <- fork go
  return $ Thread pid stopRef finishedRef

-- | Like 'create', but also apply the given label to the thread
-- (using 'labelThread').
createWithLabel :: MonadBaseControl IO m => String -> (Thread -> m ()) -> m Thread
createWithLabel label a = do
  thread <- create a
  liftBase $ labelThread (threadId thread) label
  return thread

-- | Make a thread mortal. Next time a mortal thread attempts to finish,
-- nothing will prevent it from doing so.
--
-- Calling this on an already mortalized thread has no effect.
mortalize :: Thread -> IO ()
mortalize (Thread _ stopRef _) = writeIORef stopRef True

-- | If a thread was 'mortalize'd, this will make it immortal again. However,
-- if it finished while being in the mortal state, it won't be resurrected.
--
-- Calling this on an immortal thread has no effect.
immortalize :: Thread -> IO ()
immortalize (Thread _ stopRef _) = writeIORef stopRef False

-- | Stop (kill) an immortal thread.
--
-- This is equivalent to making it mortal, and then killing it with
-- an exception.
--
-- Note that if the thread has installed its own exception handlers, it may
-- not be killed immediately.
stop :: Thread -> IO ()
stop t = do
  mortalize t
  killThread (threadId t)

-- | Wait for the thread to stop. Use 'stop' to stop the thread
wait :: Thread -> IO ()
wait = atomically . waitSTM  

-- | An STM version of 'wait'
waitSTM :: Thread -> STM ()
waitSTM (Thread _ _ finishedRef) = check =<< readTVar finishedRef

-- | Get the 'ThreadId' of the immortal thread.
--
-- The 'ThreadId' can be used to throw asynchronous exception to interrupt
-- the computation. This won't kill the thread, however — even if the
-- exception is not handled, the computation will be simply restarted.
threadId :: Thread -> ThreadId
threadId (Thread pid _ _) = pid

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
