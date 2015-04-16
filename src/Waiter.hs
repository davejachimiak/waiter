module Waiter (run) where

import Filesystem.Path.CurrentOS (encodeString, decodeString)
import System.FSNotify (withManager, watchTree, Event(..))
import System.Process (waitForProcess, callCommand, spawnCommand, readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.Process.Internals
import System.Posix.Files (fileExist)
import System.Posix.Signals (signalProcess, killProcess)
import System.Posix.Process (getProcessStatus)
import System.Posix.Types (CPid)
import Control.Monad (forever, when, unless)
import Control.Concurrent.MVar (MVar, newMVar, readMVar, takeMVar, swapMVar)
import Control.Concurrent (threadDelay, takeMVar)
import Text.Regex (mkRegex, matchRegex)
import Data.List (delete)

import Waiter.Constants
import Waiter.Types

run :: CommandLine -> IO ()
run commandLine = do
    let regexToWatch = fileRegex commandLine
        dirToWatch = decodeString $ dir commandLine

    buildPids <- newMVar []
    blockState <- newMVar False

    buildAndRun commandLine buildPids

    withManager $ \mgr -> do
        watchTree
            mgr
            dirToWatch
            (fileDoesMatch regexToWatch)
            $ blockBuildAndRun commandLine blockState buildPids

        forever getLine

buildAndRun :: CommandLine -> MVar [CPid] -> IO ()
buildAndRun commandLine buildPids = do
    newPids <- build (buildCommand commandLine) buildPids

    when (null newPids) $ stopServer (pidFile commandLine) >> startServer commandLine

build :: String -> MVar [CPid] -> IO ([CPid])
build buildCommand buildPids = do
    buildProcess <- spawnCommand buildCommand

    let (ProcessHandle openHandle _) = buildProcess

    (OpenHandle pid) <- readMVar openHandle
    existingBuildPids <- readMVar buildPids

    swapMVar buildPids $ pid : existingBuildPids
    waitForProcess buildProcess

    buildPidsAfterBuild <- readMVar buildPids

    swapMVar buildPids $ delete pid buildPidsAfterBuild

blockBuildAndRun :: CommandLine -> MVar Bool -> MVar [CPid] -> Event -> IO ()
blockBuildAndRun commandLine blockState buildPids _ = do
    doBlock <- readMVar blockState

    unless doBlock $ blockBatchEvents blockState >> buildAndRun commandLine buildPids

blockBatchEvents :: MVar Bool -> IO Bool
blockBatchEvents blockState = do
    swapMVar blockState True
    threadDelay 100000 -- microseconds: 0.1 seconds
    swapMVar blockState False

startServer :: CommandLine -> IO ()
startServer commandLine = do
    (ProcessHandle mVar _) <- spawnCommand $ serverCommand commandLine
    (OpenHandle pid) <- takeMVar mVar
    writeFile (pidFile commandLine) (show pid)

stopServer :: String -> IO ()
stopServer pidFile' = do
    pidFileDoesExist <- fileExist pidFile'

    when pidFileDoesExist $ killPid =<< readFile pidFile'

killPid :: String -> IO ()
killPid pid = do
    (exitCode, _, _) <- readProcessWithExitCode ps [pFlag, pid] emptyString

    case exitCode of
        ExitSuccess -> signalProcess killProcess (read pid :: CPid)
        ExitFailure _ -> return ()

fileDoesMatch :: String -> Event -> Bool
fileDoesMatch regexToWatch (Added fileName _) = fileDoesMatch' regexToWatch $ encodeString fileName
fileDoesMatch regexToWatch (Modified fileName _) = fileDoesMatch' regexToWatch $ encodeString fileName
fileDoesMatch regexToWatch (Removed fileName _) = fileDoesMatch' regexToWatch $ encodeString fileName

fileDoesMatch' :: String -> String -> Bool
fileDoesMatch' regexToWatch fileName =
    case matchRegex (mkRegex regexToWatch) fileName of
        Just _ -> True
        Nothing -> False
