{-
  This test checks the functionality of `ghc-events merge` and writeEventLogToFile
-}
import Control.Monad
import qualified Data.ByteString.Lazy as BL
import Data.List (( \\ ))
import Data.Maybe (fromJust)
import System.Exit (exitFailure)

import GHC.RTS.Events
import GHC.RTS.Events.Incremental (readEventLog)
import Utils (files, diffLines)

-- Failing test cases due to changes introduced some time in the past but
-- went unnoticed. Needs fixing. TODO
failingCases :: [FilePath]
failingCases = map ("test/"++)
  [ "queens-ghc-6.12.1.eventlog"
  , "queens-ghc-7.0.2.eventlog"
  , "mandelbrot-mmc-2011-06-14.eventlog"
  , "782stop.eventlog"]

rewriteLog :: EventLog -> EventLog
rewriteLog oldLog = case readEventLog (serialiseEventLog oldLog) of
  Left reason -> error reason
  Right (newLog, _) -> newLog

testFile :: FilePath -> IO Bool
testFile f = do
  e <- readEventLogFromFile f
  let oops s = putStrLn (f ++ ": failure " ++ s) >> return False
  case e of
    Left m -> oops m
    Right log -> do
      let old = ppEventLog RawTime log
      let new = ppEventLog RawTime $ rewriteLog log
      if old == new
        then putStrLn (f ++ ": success") >> return True
        else do
          putStrLn $ diffLines old new
          oops "re-written file does not match the original"

main :: IO ()
main = do
  successes <- mapM testFile files
  unless (and successes) exitFailure
