import System.Environment (getArgs)

import Waiter.Internal

main = do
    args <- getArgs

    buildAndRun args
    startWatcher args
