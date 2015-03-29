{-# LANGUAGE OverloadedStrings #-}

module Waiter.Internal (buildAndRun, setWatcher) where

import System.FSNotify (withManager, watchDir, Event(..))
import Filesystem.Path.CurrentOS (encodeString)
import System.Process (callCommand, spawnCommand)
import System.Process.Internals
import System.Posix.Files (fileExist)
import System.Posix.Signals (signalProcess, killProcess)
import System.Posix.Types (CPid)
import System.Environment (getArgs)
import Control.Monad (forever)
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
    fileExists <- fileExist pidFile

    case fileExists of
        True ->  do
            pid <- readFile pidFile
            signalProcess killProcess (read pid :: CPid)
        False -> return ()

isHaskellFile :: Event -> Bool
isHaskellFile (Added fileName _) = isHaskellFile' $ encodeString fileName
isHaskellFile (Modified fileName _) = isHaskellFile' $ encodeString fileName
isHaskellFile (Removed fileName _) = isHaskellFile' $ encodeString fileName

isHaskellFile' :: String -> Bool
isHaskellFile' fileName = 
    case matchRegex (mkRegex hsExtensionRegex) fileName of 
      Just _ -> True
      Nothing -> False
