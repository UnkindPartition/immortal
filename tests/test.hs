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

  , testCase "onFinish detects normal exit" $ do
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
