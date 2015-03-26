{-# LANGUAGE OverloadedStrings #-}

import System.FSNotify
import Filesystem.Path.CurrentOS (encodeString, decodeString)
import System.Process (rawSystem)
import System.Environment (getArgs)
import Control.Monad (forever)
import Text.Regex (mkRegex, matchRegex)

main = do
  (rawDir:command:_) <- getArgs

  let dir = decodeString rawDir

  withManager $ \mgr -> do
    _ <- watchDir mgr dir isHaskellFile $ runCommand command

    forever getLine

runCommand :: String -> Event -> IO ()
runCommand command _ = do
    rawSystem command []
    print $ "doing" ++ command

isHaskellFile :: Event -> Bool
isHaskellFile (Added file _) = isHaskellFile' $ encodeString file
isHaskellFile (Modified file _) = isHaskellFile' $ encodeString file
isHaskellFile (Removed file _) = isHaskellFile' $ encodeString file

isHaskellFile' :: String -> Bool
isHaskellFile' file = 
    case (matchRegex (mkRegex "hs$") file) of 
      Just _ -> True
      Nothing -> False
