{-# LANGUAGE OverloadedStrings #-}

module WatchAndBuild.Internal.Constants where

import Filesystem.Path.CurrentOS (decodeString)

pidFile = "./dev-server-pid"
dirToWatch = decodeString "./src"
buildCommand = "cabal build"
hsExtensionRegex = "hs$"