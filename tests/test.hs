{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Test.Tasty
import Test.Tasty.HUnit
import qualified Control.Immortal as Immortal
import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import System.Timeout

-- Almost bracket, but we don't want to start a thread inside mask
-- See http://ro-che.info/articles/2014-07-30-bracket.html
withImmortal :: IO () -> IO c -> IO c
withImmortal comp inner = do
  thread <- Immortal.create $ const comp
  inner `finally` Immortal.stop thread

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ testCase "is not killed by an exception" $ do
      tv <- atomically $ newTVar True
      immortal <- Immortal.create $ const $ keepTrue tv

      killThread (Immortal.threadId immortal)
      atomically $ writeTVar tv False
      delay
      v <- atomically $ readTVar tv
      assertBool "Thread died" v

  , testCase "never finishes" $ do
      tv <- atomically $ newTVar False
      withImmortal (keepTrue tv) $
        replicateM_ 10 $ do
          atomically $ writeTVar tv False
          delay
          v <- atomically $ readTVar tv
          assertBool "Thread died" v

  , testCase "can be stopped" $ do
      tv <- atomically $ newTVar True
      immortal <- Immortal.create $ const $ keepTrue tv

      Immortal.stop immortal
      atomically $ writeTVar tv False
      delay
      v <- atomically $ readTVar tv
      assertBool "Thread did not stop" (not v)

  , testCase "state is preserved when there are no exceptions" $ do
      tv <- atomically $ newTVar 0
      pid <- flip evalStateT 0 $ Immortal.create $ const $ countToFive tv
      (do
        delay
        v <- atomically $ readTVar tv
        v @?= 5) `finally` Immortal.stop pid

  , testCase "state is reset when there are exceptions" $ do
      tv <- atomically $ newTVar 0
      let
        computation = do
          countToFive tv
          liftIO delay
          error "bah!"
      pid <- flip evalStateT 0 $ Immortal.create $ const computation
      (do
        threadDelay (5*10^5)
        v <- atomically $ readTVar tv
        v @?= 0)
        `finally` Immortal.stop pid

  , testCase "onFinish detects normal exit" $ do
      tv <- atomically $ newTVar Nothing
      let
        comp =
          Immortal.onFinish
            (\r -> atomically $ writeTVar tv (Just r))
            (liftIO delay)
      withImmortal comp $ do
        threadDelay (2*10^5)
        v <- atomically $ readTVar tv
        case v of
          Just (Right ()) -> return ()
          _ -> assertFailure $ "unexpected result: " ++ show v

  , testCase "onFinish detects abnormal exit" $ do
      tv <- atomically $ newTVar Nothing
      let
        comp =
          Immortal.onFinish
            (\r -> atomically $ writeTVar tv (Just r))
            (do liftIO delay; error "bah!")
      withImmortal comp $ do
        threadDelay (2*10^5)
        v <- atomically $ readTVar tv
        case v of
          Just (Left (fromException -> Just (ErrorCall "bah!"))) -> return ()
          _ -> assertFailure $ "unexpected result: " ++ show v

  , testCase "mortalize allows thread to finish" $ do
      tv <- atomically $ newTVar True
      t <- Immortal.create $ const $ keepTrue tv
      Immortal.mortalize t
      atomically $ writeTVar tv False
      delay
      v1 <- atomically $ readTVar tv
      -- thread was waiting for this; v1 should be True
      v1 @?= True
      -- since the thread was mortalized, it shouldn't be restarted
      -- so try the same actions again
      atomically $ writeTVar tv False
      delay
      v2 <- atomically $ readTVar tv
      -- and we now should get False
      v2 @?= False

  , testCase "immortalize cancels mortalize" $ do
      -- this is the copy of the previous test, only after mortalize we
      -- immediately call immortalize
      tv <- atomically $ newTVar True
      t <- Immortal.create $ const $ keepTrue tv
      Immortal.mortalize t
      Immortal.immortalize t
      atomically $ writeTVar tv False
      delay
      v1 <- atomically $ readTVar tv
      v1 @?= True
      atomically $ writeTVar tv False
      delay
      v2 <- atomically $ readTVar tv
      v2 @?= True
      Immortal.stop t

  , testCase "cancelling from within the thread works" $ do
      -- tv1 checks that the thread stopped running
      -- tv2 checks that the exception was thrown
      tv1 <- atomically $ newTVar False
      tv2 <- atomically $ newTVar False
      _ <- Immortal.create $ \thread -> do
        keepTrue tv1
        Immortal.stop thread
        atomically $ writeTVar tv1 True

      delay
      atomically $ writeTVar tv1 False
      delay
      v1 <- atomically $ readTVar tv1
      v2 <- atomically $ readTVar tv2
      v1 @?= False
      v2 @?= False

  , testCase "wait is called after the thread is stopped" $ do
      thread <- Immortal.create $ \_ -> threadDelay maxBound
      _ <- forkIO $ threadDelay (10^4) >> Immortal.stop thread
      result <- timeout (10^5) $ Immortal.wait thread

      result @?= Just ()
  ]

keepTrue :: TVar Bool -> IO ()
keepTrue tv = atomically $ do
  v <- readTVar tv
  check $ not v
  writeTVar tv True

sleep :: IO ()
sleep = threadDelay (60 * 10^6) -- 1 min

delay :: IO ()
delay = threadDelay (10^5) -- 0.1 s

countToFive :: TVar Int -> StateT Int IO ()
countToFive tv = do
  n <- get
  liftIO $ atomically $ writeTVar tv n
  if n == 5
    then liftIO sleep
    else put $! n+1
