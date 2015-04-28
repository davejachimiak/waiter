module Waiter (run) where

import Filesystem.Path.CurrentOS (decodeString)
import System.FSNotify (withManager, watchTree, Event)
import System.Process (waitForProcess, terminateProcess, spawnCommand)
import System.Process.Internals (ProcessHandle(..))
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

    buildsState <- newMVar []
    blockState <- newMVar False
    serverProcess <- newEmptyMVar

    buildAndServe commandLine buildsState serverProcess

    withManager $ \mgr -> do
        watchTree
            mgr
            dirToWatch
            (fileDoesMatch regexToWatch)
            $ blockBuildAndServe commandLine blockState buildsState serverProcess

        forever getLine

buildAndServe :: CommandLine
              -> MVar [ProcessHandle]
              -> MVar ProcessHandle
              -> IO ()
buildAndServe commandLine buildsState serverProcess = do
    currentBuilds <- build (buildCommand commandLine) buildsState

    when (null currentBuilds)
        $ stopServer serverProcess
        >> startServer commandLine serverProcess

build :: String -> MVar [ProcessHandle] -> IO [ProcessHandle]
build buildCommand buildsState = do
    buildProcess <- spawnCommand buildCommand
    existingBuilds <- readMVar buildsState

    swapMVar buildsState $ buildProcess : existingBuilds
    waitForProcess buildProcess

    buildsStateAfterBuild <- readMVar buildsState

    let buildProcesses = delete buildProcess buildsStateAfterBuild

    swapMVar buildsState buildProcesses
    return buildProcesses

blockBuildAndServe :: CommandLine
                   -> MVar Bool
                   -> MVar [ProcessHandle]
                   -> MVar ProcessHandle
                   -> Event
                   -> IO ()
blockBuildAndServe commandLine blockState buildsState serverProcess _ = do
    doBlock <- readMVar blockState

    unless doBlock
        $ blockBatchEvents blockState
        >> buildAndServe commandLine buildsState serverProcess

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
