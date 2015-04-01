module Waiter.Constants where

import Filesystem.Path.CurrentOS (decodeString)

pidFile = "./dev-server-pid"
dirToWatch = decodeString "./src"
ps = "ps"
emptyString = ""
pFlag = "-p"
