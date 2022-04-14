import Control.Monad
import System.Exit

import GHC.RTS.Events
import GHC.RTS.Events.Incremental
import Utils (files)

-- | Check that an event log round-trips through encoding/decoding.
checkRoundtrip :: FilePath -> IO Bool
checkRoundtrip logFile = do
  putStrLn logFile
  Right eventlog <- readEventLogFromFile logFile
  let Right (roundtripped, _) = readEventLog $ serialiseEventLog eventlog
  if show roundtripped == show eventlog
    then return True
    else putStrLn "bad" >> return False

main :: IO ()
main = do
  successes <- mapM checkRoundtrip files
  unless (and successes) exitFailure
