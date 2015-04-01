{-# LANGUAGE OverloadedStrings #-}

module Waiter.Internal.Constants where

import Filesystem.Path.CurrentOS (decodeString)

pidFile = "./dev-server-pid"
dirToWatch = decodeString "./src"
hsExtensionRegex = "hs$"
ps = "ps"
emptyString = ""
pFlag = "-p"
