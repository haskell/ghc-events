{-
  This test checks the functionality of `ghc-events merge` and writeEventLogToFile
-}

import qualified Data.ByteString.Lazy as BL
import Data.List (( \\ ))
import Data.Maybe (fromJust)
import System.Exit (exitFailure)

import GHC.RTS.Events
import GHC.RTS.EventsIncremental
import Utils (files, diffLines)


-- Failing test cases due to changes introduced some time in the past but
-- went unnoticed. Needs fixing. TODO
failingCases = map ("test/"++)
  [ "queens-ghc-6.12.1.eventlog"
  , "queens-ghc-7.0.2.eventlog"
  , "mandelbrot-mmc-2011-06-14.eventlog"
  , "782stop.eventlog"]

-- Returns a pretty printed version of the log and one that's been reserialised
-- and reparsed, which should yield the same result
rewriteLog :: EventLog -> (String, String)
rewriteLog oldLog =
  (ppEventLog oldLog, ppEventLog newLog)
  where
    logBytestring = serialiseEventLog oldLog
    eps = newParserState `pushBytes` (BL.toStrict logBytestring)
    (newEvts, finalState, _) = readRemainingEvents eps
    newHdr = fromJust $ readHeader finalState
    newLog = (EventLog newHdr (Data newEvts))

testFile :: FilePath -> IO Bool
testFile f = do
    e <- readEventLogFromFile f
    let oops s = putStrLn (f ++ ": failure " ++ s) >> return False
    case e of
        Left m -> oops m
        Right log -> do
          let (old, new) = rewriteLog log
          if old == new
                    then putStrLn (f ++ ": success") >> return True
                    else do putStrLn $ diffLines old new
                            oops "re-written file does not match the original"

main :: IO ()
main = do
    successes <- mapM testFile files
    if and successes
        then return ()
        else exitFailure
