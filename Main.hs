module Main where

import Control.Concurrent
import Control.Monad (replicateM)
import System.Random (randomRIO)
-- import Control.Concurrent.Async

mVarTest :: IO ()
mVarTest = let 
    action m (w, i) = do 
        threadDelay $ w * 1000000
        putStrLn $ "Thread #" ++ show i ++ " finished in " ++ show w ++ " seconds"
        putMVar m w
    in do
        m <- newEmptyMVar
        caps <- getNumCapabilities
        putStrLn $ "Running " ++ show caps ++ " threads" 
        delays <- replicateM caps $ randomRIO (1, 12)
        mapM_ (forkIO . action m) $ zip delays [1..]
        results <- replicateM caps $ takeMVar m
        putStrLn $ "Results: " ++ show results


main :: IO ()
main = mVarTest