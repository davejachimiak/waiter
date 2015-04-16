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
    let fileRegex' = fileRegex commandLine
        dirToWatch = decodeString $ dir commandLine

    buildsState <- newMVar []
    blockState <- newMVar False

    buildAndRun commandLine buildsState

    withManager $ \mgr -> do
        watchTree
            mgr
            dirToWatch
            (fileDoesMatch fileRegex')
            $ buildAndRun' commandLine blockState buildsState

        forever getLine

buildAndRun :: CommandLine -> MVar [CPid] -> IO ()
buildAndRun commandLine buildsState = do
    processHandle <- spawnCommand $ buildCommand commandLine

    let (ProcessHandle mVar _) = processHandle

    (OpenHandle pid) <- readMVar mVar
    buildPids' <- readMVar buildsState
    swapMVar buildsState $ pid : buildPids'
    waitForProcess processHandle
    buildPids'' <- readMVar buildsState

    let newPids = delete pid buildPids''

    swapMVar buildsState newPids

    when (null newPids) $ do
        stopServer $ pidFile commandLine
        startServer commandLine

buildAndRun' :: CommandLine -> MVar Bool -> MVar [CPid] -> Event -> IO ()
buildAndRun' commandLine blockState buildsState _ = do
    block <- readMVar blockState

    unless block $ blockBatchEvents blockState >> buildAndRun commandLine buildsState

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
fileDoesMatch fileRegex' (Added fileName _) = fileDoesMatch' fileRegex' $ encodeString fileName
fileDoesMatch fileRegex' (Modified fileName _) = fileDoesMatch' fileRegex' $ encodeString fileName
fileDoesMatch fileRegex' (Removed fileName _) = fileDoesMatch' fileRegex' $ encodeString fileName

fileDoesMatch' :: String -> String -> Bool
fileDoesMatch' fileRegex' fileName =
    case matchRegex (mkRegex fileRegex') fileName of
        Just _ -> True
        Nothing -> False
