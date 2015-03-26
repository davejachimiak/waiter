{-# LANGUAGE OverloadedStrings #-}

import System.FSNotify
import System.Process (rawSystem)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)

main =
  withManager $ \mgr -> do
    watchDir
      mgr          -- manager
      "."          -- directory to watch
      (const True) -- predicate
      (\x -> print x) -- action

    -- sleep forever (until interrupted)
    forever $ threadDelay maxBound
