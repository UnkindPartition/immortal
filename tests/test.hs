import Test.Tasty
import Test.Tasty.HUnit
import qualified Control.Immortal as Immortal
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Trans.State
import Control.Monad.IO.Class

main = defaultMain $ testGroup "Tests"
  [ testCase "is not killed by an exception" $ do
      tv <- atomically $ newTVar True
      immortal <- Immortal.create $ keepTrue tv

      killThread (Immortal.threadId immortal)
      atomically $ writeTVar tv False
      delay
      v <- atomically $ readTVar tv
      assertBool "Thread died" v

  , testCase "never finishes" $ do
      tv <- atomically $ newTVar False
      immortal <- Immortal.create $ keepTrue tv

      replicateM_ 10 $ do
        atomically $ writeTVar tv False
        delay
        v <- atomically $ readTVar tv
        assertBool "Thread died" v

  , testCase "can be stopped" $ do
      tv <- atomically $ newTVar True
      immortal <- Immortal.create $ keepTrue tv

      Immortal.stop immortal
      atomically $ writeTVar tv False
      delay
      v <- atomically $ readTVar tv
      assertBool "Thread did not stop" (not v)

  , testCase "state is preserved when there are no exceptions" $ do
      tv <- atomically $ newTVar 0
      immortal <- flip evalStateT 0 $ Immortal.create $ countToFive tv
      delay
      v <- atomically $ readTVar tv
      v @?= 5

  , testCase "state is reset when there are exceptions" $ do
      tv <- atomically $ newTVar 0
      immortal <- flip evalStateT 0 $ Immortal.create $ do
        countToFive tv
        liftIO delay
        error "bah!"
      threadDelay (5*10^5)
      v <- atomically $ readTVar tv
      v @?= 0
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
