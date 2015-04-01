import Options.Applicative
import Options.Applicative.Types
import System.Environment (getArgs)

import Waiter (buildAndRun, startWatcher)
import Waiter.Types (CommandLine(..))

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
    <*> strOption
        ( long "file-name-regex"
        <> value ".*"
        <> short 'f'
        <> metavar "REGEX"
        <> help "A rebuild is triggered for changes in files whose names match this regex. Default: .*" )
    <*> strOption
        ( long "dir"
        <> short 'd'
        <> value "./src"
        <> metavar "DIR"
        <> help "Look for file changes in this directory. Default: ./src" )
    <*> strOption
        ( long "pid-file"
        <> short 'p'
        <> value "./.dev-server-pid"
        <> metavar "PID_FILE"
        <> help "File to store server pid. Default: ./.dev-server-pid" )

opts :: ParserInfo CommandLine
opts = info (cli <**> helper) idm
