-- | Development version to be run inside GHCi.

module DevelMain where

import           Control.Concurrent
import           Control.Exception                (finally)
import           Data.IORef
import           Foreign.Store
import           Language.Javascript.JSaddle.Warp
import           Main                             (app)

-- | Start or restart the server.
update :: IO ()
update = do
    mtidStore <- lookupStore tid_1
    case mtidStore of
      Nothing -> do
          done <- newEmptyMVar
          _done_0 <- newStore done
          tid <- start done
          tidRef <- newIORef tid
          _tid_1 <- newStore tidRef
          return ()
      Just tidStore -> do
          tidRef <- readStore tidStore
          tid <- readIORef tidRef
          done <- readStore (Store done_0)
          killThread tid
          takeMVar done
          newTid <- start done
          writeIORef tidRef newTid
  where tid_1 = 1
        done_0 = 0

-- | Start the server in a separate thread.
start :: MVar () -- ^ Written to when the thread is killed.
      -> IO ThreadId
start done = do
    forkIO (finally (run 3000 app)
                    (putMVar done ()))
