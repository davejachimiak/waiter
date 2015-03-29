{-# LANGUAGE OverloadedStrings #-}

{-import Debug.Trace-}

import System.FSNotify
import Filesystem.Path.CurrentOS (encodeString, decodeString)
import System.Process
import System.Process.Internals
import System.Posix.Signals
import System.Posix.Types
import System.Environment (getArgs)
import Control.Monad (forever)
import Control.Concurrent.MVar
import Text.Regex (mkRegex, matchRegex)

pidFile = "./dev-server-pid"

dirToWatch = "./src"

buildCommand = "cabal build"

main = do
    startProgram
    watchBuildAndRun

watchBuildAndRun :: IO ()
watchBuildAndRun =
    withManager $ \mgr -> do
        _ <- watchDir mgr dirToWatch isHaskellFile buildAndRun

        forever getLine

buildAndRun :: Event -> IO ()
buildAndRun _ = do
    callCommand buildCommand
    stopProgram
    startProgram

startProgram :: IO ()
startProgram = do
    (binary:_) <- getArgs
    (ProcessHandle mVar _) <- spawnCommand binary
    (OpenHandle pid) <- takeMVar mVar
    writeFile pidFile $ show pid

stopProgram :: IO ()
stopProgram = do
    pid <- readFile pidFile
    signalProcess killProcess (read pid :: CPid)

isHaskellFile :: Event -> Bool
isHaskellFile (Added fileName _) = isHaskellFile' $ encodeString fileName
isHaskellFile (Modified fileName _) = isHaskellFile' $ encodeString fileName
isHaskellFile (Removed fileName _) = isHaskellFile' $ encodeString fileName

isHaskellFile' :: String -> Bool
isHaskellFile' fileName = 
    case matchRegex (mkRegex "hs$") fileName of 
      Just _ -> True
      Nothing -> False
