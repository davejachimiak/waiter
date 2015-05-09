module Waiter.BuildAndServe (buildAndServe) where

import System.Process (waitForProcess, terminateProcess, spawnCommand)
import System.FSNotify (Event)
import Control.Concurrent.MVar (MVar, putMVar, tryTakeMVar)
import System.Process.Internals (ProcessHandle(..))
import System.Exit (ExitCode(..))

import Waiter.Types

class BuildAndServe a where
    buildAndServe :: CommandLine
                  -> CurrentBuild
                  -> ServerProcess
                  -> a
                  -> IO ()

    buildAndServe commandLine currentBuild serverProcess _ = do
        maybeTerminateProcessFromMVar currentBuild

        startBuild (buildCommand commandLine) currentBuild
            >>= waitForProcess
            >>= runServer commandLine serverProcess

instance BuildAndServe Event
instance BuildAndServe ProgramStart

maybeTerminateProcessFromMVar :: MVar ProcessHandle -> IO ()
maybeTerminateProcessFromMVar processMVar = do
    process <- tryTakeMVar processMVar

    case process of
        Just process -> terminateProcess process
        Nothing -> return ()

startServer :: CommandLine -> ServerProcess -> IO ()
startServer commandLine serverProcess = do
    putMVar serverProcess =<< spawnCommand (serverCommand commandLine)

startBuild :: String -> CurrentBuild -> IO ProcessHandle
startBuild buildCommand currentBuild = do
    build <- spawnCommand buildCommand
    putMVar currentBuild build
    return build

runServer :: CommandLine -> ServerProcess -> ExitCode -> IO ()
runServer commandLine serverProcess ExitSuccess = do
    maybeTerminateProcessFromMVar serverProcess
    startServer commandLine serverProcess
runServer _ _ _ = return ()
