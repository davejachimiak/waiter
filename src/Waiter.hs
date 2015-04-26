module Waiter (run) where

import Filesystem.Path.CurrentOS (decodeString)
import System.FSNotify (withManager, watchTree, Event)
import System.Process (waitForProcess, terminateProcess, spawnCommand, callCommand)
import System.Process.Internals (ProcessHandle(..))
import Control.Monad (forever, unless)
import Control.Concurrent.MVar
import Control.Concurrent (threadDelay)
import Text.Regex (mkRegex, matchRegex)
import Data.List (delete)

import Waiter.Types 
import Waiter.FileMatcher (fileDoesMatch)

run :: CommandLine -> IO ()
run commandLine = do
    let regexToWatch = fileRegex commandLine
        dirToWatch = decodeString $ dir commandLine

    blockState <- newMVar False
    serverProcess <- newEmptyMVar

    buildAndServe commandLine serverProcess

    withManager $ \mgr -> do
        watchTree
            mgr
            dirToWatch
            (fileDoesMatch regexToWatch)
            $ blockBuildAndServe commandLine blockState serverProcess

        forever getLine

buildAndServe :: CommandLine -> MVar ProcessHandle -> IO ()
buildAndServe commandLine serverProcess = do
    callCommand $ buildCommand commandLine
    stopServer serverProcess
    startServer commandLine serverProcess

blockBuildAndServe :: CommandLine
                   -> MVar Bool
                   -> MVar ProcessHandle
                   -> Event
                   -> IO ()
blockBuildAndServe commandLine blockState serverProcess _ = do
    doBlock <- readMVar blockState

    unless doBlock
        $ blockBatchEvents blockState
        >> buildAndServe commandLine serverProcess

blockBatchEvents :: MVar Bool -> IO Bool
blockBatchEvents blockState = do
    swapMVar blockState True
    threadDelay 100000 -- microseconds: 0.1 seconds
    swapMVar blockState False

startServer :: CommandLine -> MVar ProcessHandle -> IO ()
startServer commandLine serverProcess = do
    newServerProcess <- spawnCommand $ serverCommand commandLine
    putMVar serverProcess newServerProcess
    return ()

stopServer :: MVar ProcessHandle -> IO ()
stopServer serverProcess = do
    result <- tryTakeMVar serverProcess

    case result of
        Just process -> terminateProcess process
        Nothing -> return ()
