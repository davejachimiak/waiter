module Waiter (buildAndRun, startWatcher) where

import System.FSNotify (withManager, watchDir, Event(..))
import Filesystem.Path.CurrentOS (encodeString, decodeString)
import System.Process (callCommand, spawnCommand, readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.Process.Internals
import System.Posix.Files (fileExist)
import System.Posix.Signals (signalProcess, killProcess)
import System.Posix.Process (getProcessStatus)
import System.Posix.Types (CPid)
import Control.Monad (forever, when)
import Control.Concurrent.MVar (takeMVar)
import Text.Regex (mkRegex, matchRegex)

import Waiter.Constants
import Waiter.Types

startWatcher :: CommandLine -> IO ()
startWatcher commandLine = do
    let fileRegex' = fileRegex commandLine
        dirToWatch = decodeString $ dir commandLine

    withManager $ \mgr -> do
        watchDir mgr dirToWatch (fileDoesMatch fileRegex') (buildAndRun' commandLine)

        forever getLine

buildAndRun :: CommandLine -> IO ()
buildAndRun commandLine = do
    callCommand $ buildCommand commandLine
    stopServer $ pidFile commandLine
    startServer commandLine

buildAndRun' :: CommandLine -> Event -> IO ()
buildAndRun' commandLine _ = buildAndRun commandLine

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