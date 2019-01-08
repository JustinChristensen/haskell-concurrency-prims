module Main where

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import GHC.Word (Word32)
import GHC.Stats (
    getRTSStats,
    gc,
    gcdetails_threads)
import Control.Concurrent
import Control.Monad (replicateM)
import System.Random (randomRIO)
import Control.Concurrent.Async (
    mapConcurrently_,
    mapConcurrently,
    async, wait)

getOSThreadCount :: IO Word32
getOSThreadCount = gcdetails_threads . gc <$> getRTSStats

putOSThreadCount :: IO ()
putOSThreadCount = getOSThreadCount >>= \tc ->
    putStrLn $ "Thread Count" ++ show tc

delays :: Int -> IO [(Int, Int)]
delays n = do
    -- fork n number of threads with random delays between
    -- 1 and 12 seconds
    ds <- replicateM n $ randomRIO (1, 12)
    return $ zip ds [1..]

logResults :: Show a => a -> IO ()
logResults a = putStrLn $ "Results: " ++ show a

threadAction :: (Int, Int) -> IO ()
threadAction (w, i) = do
    putStrLn $ "Thread #" ++ show i ++ " started"

    -- delay the thread for w seconds (simulate a long running computation)
    threadDelay $ w * 1000000

    -- By default IO in parallel threads is unbuffered and interleaved
    putStrLn $ "Thread #" ++ show i ++ " finished in " ++ show w ++ " seconds"

-- an MVar is a synchronized shared area of memory
-- if the MVar is empty, takeMVar will block until it becomes full
-- if the MVar is full, takeMVar will empty it and return the value
-- if the MVar is empty, putMVar will set the value
-- if the MVar is full, putMVar will block until it becomes empty
basicMVarTest :: IO ()
basicMVarTest = let
    action m (w, i) = do
        threadAction (w, i)

         -- wait until the shared memory becomes available to write the result
         -- of this long-running computation
         -- in effect, this thread enters a state of being "ready to write"
        putMVar m w
        -- after writing, the thread can exit
    in do
        putStrLn "basicMVarTest"

        -- one single mvar
        m <- newEmptyMVar

        -- get the number of OS threads available to the runtime
        caps <- getNumCapabilities

        -- fork cap number of threads with random delays between
        -- 1 and 12 seconds
        ds <- delays caps
        mapM_ (forkIO . action m) ds

        putStrLn $ "Running " ++ show caps ++ " threads"

        -- we know we've got caps number of threads that are each
        -- going to write a result to the mvar created above (unless error)
        -- this is going to read the results in the order they're written
        -- by the forked threads
        results <- replicateM caps $ takeMVar m

        -- once all of the results have been written we can print them out
        logResults results

basicMapConcurrentlyTest :: IO ()
basicMapConcurrentlyTest = let
        action (w, i) = threadAction (w, i) >> return w
    in do
        putStrLn "basicMapConcurrentlyTest"
        ds <- delays 40
        results <- mapConcurrently action ds
        logResults results

multiMapConcurrentlyTest :: IO ()
multiMapConcurrentlyTest = let
        action (w, i) = threadAction (w, i) >> return w
        multiMapConcurrentlyTest' _ [] = return []
        multiMapConcurrentlyTest' n ds = do
            let (batch, rest) = splitAt n ds
            rs <- mapConcurrently action batch
            rss <- multiMapConcurrentlyTest' n rest
            return $ rs : rss
    in do
        putStrLn "multiMapConcurrentlyTest"
        ds <- delays 40
        results <- multiMapConcurrentlyTest' 8 ds
        logResults $ concat results

-- collect results in the order that async's are created
-- async N times
-- as async finishes, notify channel
-- on channel notification, create next async

basicAsyncTest :: IO ()
basicAsyncTest = let
        action (w, i) = threadAction (w, i) >> return w
    in do
        putStrLn "basicAsyncTest"
        ds <- delays 2
        a1 <- async $ action $ ds !! 0
        a2 <- async $ action $ ds !! 1
        r1 <- wait a1
        r2 <- wait a2
        logResults [r1, r2]

main :: IO ()
main = let
        mHead [] = Nothing
        mHead xs = Just $ head xs
        formatChoices = unlines . map (\(n, c) -> show n ++ ". " ++ c) . zip [1..]
        choices = [
            "basicMVarTest",
            "basicMapConcurrentlyTest",
            "multiMapConcurrentlyTest",
            "basicAsyncTest"]
        tests = [
            basicMVarTest,
            basicMapConcurrentlyTest,
            multiMapConcurrentlyTest,
            basicAsyncTest]
        usage = do
            name <- getProgName
            putStrLn $ "usage: " ++ name ++ " [-h|-N]"
            putStrLn "-h\tPrint help"
            putStrLn "-N\tRun test #N\n"
            putStrLn "Choices:"
            putStrLn $ formatChoices choices
            exitFailure
    in do
        args <- getArgs
        case mHead args of
            Just "-h" -> usage
            Just narg -> do
                let i = pred $ read $ tail narg :: Int
                if i `elem` [0..pred $ length tests] then
                    tests !! i
                else 
                    usage
            Nothing -> sequence_ tests