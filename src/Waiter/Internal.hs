{-# LANGUAGE OverloadedStrings #-}

module Waiter.Internal (buildAndRun, setWatcher) where

import System.FSNotify (withManager, watchDir, Event(..))
import Filesystem.Path.CurrentOS (encodeString)
import System.Process (callCommand, spawnCommand, system)
import System.Exit (ExitCode(..))
import System.Process.Internals
import System.Posix.Files (fileExist)
import System.Posix.Signals (signalProcess, killProcess)
import System.Posix.Process (getProcessStatus)
import System.Posix.Types (CPid)
import System.Environment (getArgs)
import Control.Monad (forever, when)
import Control.Concurrent.MVar (takeMVar)
import Text.Regex (mkRegex, matchRegex)

import Waiter.Internal.Constants

setWatcher :: IO ()
setWatcher = withManager $ \mgr -> do
    watchDir mgr dirToWatch isHaskellFile buildAndRun_

    forever getLine

buildAndRun :: IO ()
buildAndRun = do
    callCommand buildCommand
    stopServer
    startServer

buildAndRun_ :: Event -> IO ()
buildAndRun_ _ = buildAndRun

startServer :: IO ()
startServer = do
    (binary:_) <- getArgs
    (ProcessHandle mVar _) <- spawnCommand binary
    (OpenHandle pid) <- takeMVar mVar
    writeFile pidFile $ show pid

stopServer :: IO ()
stopServer = do
    pidFileDoesExist <- fileExist pidFile

    when pidFileDoesExist $ do
        pid <- readFile pidFile

        killPid pid

killPid :: String -> IO ()
killPid pid = do
    exitCode <- system $ checkProcessPrefix ++ pid

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
