{-# LANGUAGE OverloadedStrings #-}

import Options.Applicative
import Options.Applicative.Types
import System.Environment (getArgs)

import Waiter.Internal (buildAndRun, startWatcher)
import Waiter.Types (CommandLine)

main :: IO ()
main = run =<< execParser opts

run :: CommandLine -> IO ()
run commandLine = do
    buildAndRun commandLine
    startWatcher commandLine

cli :: Parser CommandLine
cli = CommandLine
    <$> argument str 
        ( metavar "SERVER_COMMAND"
        <> help "the command to run your server" )
    <*> argument str
        ( metavar "BUILD_COMMAND"
        <> help "the command to build your server" )

opts :: ParserInfo CommandLine
opts = info (cli <**> helper) idm
