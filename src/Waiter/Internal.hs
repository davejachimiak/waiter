{-# LANGUAGE OverloadedStrings #-}

module Waiter.Internal (buildAndRun, startWatcher) where

import System.FSNotify (withManager, watchDir, Event(..))
import Filesystem.Path.CurrentOS (encodeString)
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

import Waiter.Internal.Constants
import Waiter.Types

startWatcher :: CommandLine -> IO ()
startWatcher commandLine = withManager $ \mgr -> do
    watchDir mgr dirToWatch isHaskellFile (buildAndRun_ commandLine)

    forever getLine

buildAndRun :: CommandLine -> IO ()
buildAndRun commandLine = do
    callCommand $ buildCommand commandLine
    stopServer
    startServer commandLine

buildAndRun_ :: CommandLine -> Event -> IO ()
buildAndRun_ commandLine _ = buildAndRun commandLine

startServer :: CommandLine -> IO ()
startServer commandLine = do
    (ProcessHandle mVar _) <- spawnCommand $ serverCommand commandLine
    (OpenHandle pid) <- takeMVar mVar
    writeFile pidFile $ show pid

stopServer :: IO ()
stopServer = do
    pidFileDoesExist <- fileExist pidFile

    when pidFileDoesExist $ killPid =<< readFile pidFile

killPid :: String -> IO ()
killPid pid = do
    (exitCode, _, _) <- readProcessWithExitCode ps [pFlag, pid] emptyString

    case exitCode of
        ExitSuccess -> signalProcess killProcess (read pid :: CPid)
        ExitFailure _ -> return ()

isHaskellFile :: Event -> Bool
isHaskellFile (Added fileName _) = isHaskellFile' $ encodeString fileName
isHaskellFile (Modified fileName _) = isHaskellFile' $ encodeString fileName
isHaskellFile (Removed fileName _) = isHaskellFile' $ encodeString fileName

isHaskellFile' :: String -> Bool
isHaskellFile' fileName = 
    case matchRegex (mkRegex hsExtensionRegex) fileName of 
        Just _ -> True
        Nothing -> False
