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
  , onUnexpectedFinish
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad.IO.Unlift
import Data.IORef
import GHC.Conc (labelThread)

-- | Immortal thread identifier (including its underlying 'ThreadId')
data Thread = Thread ThreadId (IORef Bool) (TVar Bool)

-- | Spawn a new immortal thread running the given computation.
--
-- If the computation ever finishes (either normally or due to an exception),
-- it will be restarted (in the same thread).
create
  :: MonadUnliftIO m
  => (Thread -> m ())
  -> m Thread
create a = withRunInIO $ \run -> uninterruptibleMask $ \restore -> do
  -- Why use uninterruptibleMask instead of just mask? We're not using any
  -- blocking operations so far, so there should be no difference. Still,
  -- better be safe than sorry.
  stopRef     <- newIORef  False
  finishedRef <- newTVarIO False
  let
    go = do
      -- construct a thread object from within the thread itself
      pid <- myThreadId
      let thread = Thread pid stopRef finishedRef

      handle (\(_ :: SomeException) -> return ()) (restore $ run $ a thread)

      stopNow <- readIORef stopRef
      if stopNow then
        atomically $ writeTVar finishedRef True
      else
        go
  pid <- forkIO go
  return $ Thread pid stopRef finishedRef

-- | Like 'create', but also apply the given label to the thread
-- (using 'labelThread').
createWithLabel :: MonadUnliftIO m => String -> (Thread -> m ()) -> m Thread
createWithLabel label a = do
  thread <- create a
  liftIO $ labelThread (threadId thread) label
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
-- >Immortal.create $ \_ -> Immortal.onFinish print myAction
--
-- This is nothing more than a simple wrapper around 'try'.
onFinish
  :: MonadUnliftIO m
  => (Either SomeException () -> m ())
  -> m () -> m ()
onFinish cb a = withRunInIO $ \run -> try (run a) >>= run . cb

-- | Like 'onFinish', but the callback does not run when the thread is
-- mortalized (i.e. when the exit is expected).
--
-- The 'Thread' argument is used to find out the mortality of the thread.
onUnexpectedFinish
  :: MonadUnliftIO m
  => Thread
  -> (Either SomeException () -> m ())
  -> m ()
  -> m ()
onUnexpectedFinish (Thread _ stopRef _) cb a = withRunInIO $ \run -> do
  r <- try $ run a
  expected <- readIORef stopRef
  if expected
    then return ()
    else run $ cb r
