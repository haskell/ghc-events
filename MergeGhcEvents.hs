{-# OPTIONS_GHC -funbox-strict-fields #-}

import GHC.RTS.Events
import GHC.RTS.Events.Merge
import System.Environment

-- todo, proper error handling
main = do
    [out, file1, file2] <- getArgs
    Right f1 <- readEventLogFromFile file1
    Right f2 <- readEventLogFromFile file2
    let m = mergeEventLogs f1 f2
    writeEventLogToFile out m
