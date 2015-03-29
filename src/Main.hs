{-# LANGUAGE OverloadedStrings #-}

{-import Debug.Trace-}

import System.FSNotify
import Filesystem.Path.CurrentOS (encodeString, decodeString)
import System.Process
import System.Process.Internals
import System.Posix.Signals
import System.Environment (getArgs)
import Control.Monad (forever)
import Text.Regex (mkRegex, matchRegex)

pidFile = "./dev-server-pid"

dirToWatch = "./src"

buildCommand = "cabal build"

main = do
    startProgram
    watchBuildAndRun

watchBuildAndRun :: IO ()
watchBuildAndRun = do
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
    OpenHandle pid <- spawnCommand binary
    writeFile pidFile (show pid)

stopProgram :: IO ()
stopProgram = do
    pid <- readFile pidFile
    signalProcess killProcess (read pid :: Int)

isHaskellFile :: Event -> Bool
isHaskellFile (Added file _) = isHaskellFile' $ encodeString file
isHaskellFile (Modified file _) = isHaskellFile' $ encodeString file
isHaskellFile (Removed file _) = isHaskellFile' $ encodeString file

isHaskellFile' :: String -> Bool
isHaskellFile' file = 
    case (matchRegex (mkRegex "hs$") file) of 
      Just _ -> True
      Nothing -> False
