{-
  This test checks the functionality of `ghc-events merge` and writeEventLogToFile
-}

import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromJust)
import System.Exit (exitFailure)

import GHC.RTS.Events
import GHC.RTS.EventsIncremental


files :: [FilePath]
files = map ("test/"++)
    [ "queens-ghc-6.12.1.eventlog"
    , "queens-ghc-7.0.2.eventlog"
    , "mandelbrot-mmc-2011-06-14.eventlog"
    , "mdlLogMPI1.eventlog"
    , "pre77stop.eventlog", "782stop.eventlog", "783stop.eventlog" ]

testWrite :: EventLog -> Bool
testWrite oldLog =
  ppEventLog oldLog == ppEventLog newLog
  where
    logBytestring = serialiseEventLog oldLog
    eps = newParser `pushBytes` (BL.toStrict logBytestring)
    (newEvts, finalState, _) = readRemainingEvents eps
    newHdr = fromJust $ readHeader finalState
    newLog = (EventLog newHdr (Data newEvts))

-- returns True on success
testFile :: FilePath -> IO Bool
testFile f = do
    e <- readEventLogFromFile f
    let oops s = putStrLn (f ++ ": failure " ++ s) >> return False
    case e of
        Left m -> oops m
        Right log -> do
          if testWrite log
                    then putStrLn (f ++ ": success") >> return True
                    else do oops "re-written file does not match the original"

main :: IO ()
main = do
    successes <- mapM testFile files
    if and successes
        then return ()
        else exitFailure
