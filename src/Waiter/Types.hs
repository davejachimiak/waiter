module Waiter.Types where

import Control.Concurrent.MVar (MVar)
import System.Process.Internals (ProcessHandle(..))

type ServerProcess = MVar ProcessHandle
type CurrentBuild = MVar ProcessHandle

data ProgramStart = ProgramStart

data CommandLine = CommandLine
    { serverCommand :: String
    , buildCommand :: String
    , fileRegex :: String
    , dir :: String }
