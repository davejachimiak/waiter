{-# LANGUAGE OverloadedStrings #-}

module Waiter.Internal.Constants where

import Filesystem.Path.CurrentOS (decodeString)

pidFile = "./dev-server-pid"
dirToWatch = decodeString "./src"
buildCommand = "cabal build"
hsExtensionRegex = "hs$"
ps = "ps"
emptyString = ""
pFlag = "-p"
