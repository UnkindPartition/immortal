immortal
========

A small library to create threads that never die. This is useful e.g. for
writing servers.

``` haskell
import qualified System.Immortal as Immortal
import Control.Concurrent (threadDelay)
import Control.Monad (forever)

main = do
  -- start an immortal thread
  _thread <- Immortal.create $ \ _thread -> do
    -- do stuff

  -- in the main thread, sleep until interrupted
  -- (e.g. with Ctrl-C)
  forever $ threadDelay maxBound
```
