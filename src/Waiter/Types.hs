module Waiter.Types where

data CommandLine = CommandLine
    { serverCommand :: String
    , buildCommand :: String
    , fileRegex :: String
    , dir :: String
    , pidFile :: String }
