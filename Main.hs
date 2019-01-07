module Main where

import GHC.Word (Word32)
import GHC.Stats (
    getRTSStats, 
    gc, 
    gcdetails_threads)
import Control.Concurrent
import Control.Monad (replicateM)
import System.Random (randomRIO)
-- import Control.Concurrent.Async

-- an MVar is a synchronized shared area of memory
-- if the MVar is empty, takeMVar will block until it becomes full
-- if the MVar is full, takeMVar will empty it and return the value
-- if the MVar is empty, putMVar will set the value
-- if the MVar is full, putMVar will block until it becomes empty

getOSThreadCount :: IO Word32
getOSThreadCount = gcdetails_threads . gc <$> getRTSStats

putOSThreadCount :: IO ()
putOSThreadCount = getOSThreadCount >>= \tc -> 
    putStrLn $ "Thread Count" ++ show tc

basicMVarTest :: IO ()
basicMVarTest = let
    action m (w, i) = do
        putStrLn $ "Thread #" ++ show i ++ " started"

        -- delay the thread for w seconds (simulate a long running computation)
        threadDelay $ w * 1000000

        -- By default IO in parallel threads is unbuffered and interleaved
        putStrLn $ "Thread #" ++ show i ++ " finished in " ++ show w ++ " seconds"

         -- wait until the shared memory becomes available to write the result
         -- of this long-running computation
         -- in effect, this thread enters a state of being "ready to write"
        putMVar m w
        -- after writing, the thread can exit
    in do
        -- one single mvar
        m <- newEmptyMVar

        -- get the number of OS threads available to the runtime
        caps <- getNumCapabilities
        putStrLn $ "Running " ++ show caps ++ " threads"

        -- fork cap number of threads with random delays between
        -- 1 and 12 seconds
        delays <- replicateM caps $ randomRIO (1, 12)
        mapM_ (forkIO . action m) $ zip delays [1..]

        -- we know we've got caps number of threads that are each
        -- going to write a result to the mvar created above (unless error)
        -- this is going to read the results in the order they're written
        -- by the forked threads
        results <- replicateM caps $ takeMVar m

        -- once all of the results have been written we can print them out
        putStrLn $ "Results: " ++ show results

main :: IO ()
main = basicMVarTest