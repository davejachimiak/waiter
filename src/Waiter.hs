module Waiter (run) where

import Filesystem.Path.CurrentOS (encodeString, decodeString)
import System.FSNotify (withManager, watchTree, Event(..))
import System.Process (waitForProcess, terminateProcess, spawnCommand)
import System.Process.Internals
import System.Posix.Types (CPid)
import Control.Monad (forever, when, unless)
import Control.Concurrent.MVar
import Control.Concurrent (threadDelay)
import Text.Regex (mkRegex, matchRegex)
import Data.List (delete)

import Waiter.Types

run :: CommandLine -> IO ()
run commandLine = do
    let regexToWatch = fileRegex commandLine
        dirToWatch = decodeString $ dir commandLine

    buildPids <- newMVar []
    blockState <- newMVar False
    serverProcess <- newEmptyMVar

    buildAndRun commandLine buildPids serverProcess

    withManager $ \mgr -> do
        watchTree
            mgr
            dirToWatch
            (fileDoesMatch regexToWatch)
            $ blockBuildAndRun commandLine blockState buildPids serverProcess

        forever getLine

buildAndRun :: CommandLine -> MVar [CPid] -> MVar ProcessHandle -> IO ()
buildAndRun commandLine buildPids serverProcess = do
    currentBuildPids <- build (buildCommand commandLine) buildPids

    when (null currentBuildPids)
        $ stopServer serverProcess
        >> startServer commandLine serverProcess

build :: String -> MVar [CPid] -> IO [CPid]
build buildCommand buildPids = do
    buildProcess <- spawnCommand buildCommand

    let (ProcessHandle openHandle _) = buildProcess

    (OpenHandle pid) <- readMVar openHandle
    existingBuildPids <- readMVar buildPids

    swapMVar buildPids $ pid : existingBuildPids
    waitForProcess buildProcess

    buildPidsAfterBuild <- readMVar buildPids

    let newPids = delete pid buildPidsAfterBuild

    swapMVar buildPids newPids
    return newPids

blockBuildAndRun :: CommandLine -> MVar Bool -> MVar [CPid] -> MVar ProcessHandle -> Event -> IO ()
blockBuildAndRun commandLine blockState buildPids serverProcess _ = do
    doBlock <- readMVar blockState

    unless doBlock $ blockBatchEvents blockState >> buildAndRun commandLine buildPids serverProcess

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

fileDoesMatch :: String -> Event -> Bool
fileDoesMatch regexToWatch (Added fileName _) = fileDoesMatch' regexToWatch $ encodeString fileName
fileDoesMatch regexToWatch (Modified fileName _) = fileDoesMatch' regexToWatch $ encodeString fileName
fileDoesMatch regexToWatch (Removed fileName _) = fileDoesMatch' regexToWatch $ encodeString fileName

fileDoesMatch' :: String -> String -> Bool
fileDoesMatch' regexToWatch fileName =
    case matchRegex (mkRegex regexToWatch) fileName of
        Just _ -> True
        Nothing -> False
