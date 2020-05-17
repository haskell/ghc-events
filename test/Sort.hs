import Control.Monad
import System.Exit
import System.FilePath
import System.IO.Temp

import GHC.RTS.Events
import qualified GHC.RTS.Events.Sort as Sort
import Utils (files, diffLines)

-- | This is chosen to be small to ensure that we tickle the merge sort path.
sortParams :: Sort.SortParams
sortParams = Sort.SortParams { chunkSize = 1000, maxFanIn = 10 }

-- | Check that merge sort computes the same result as in-memory sort.
checkSort :: FilePath -> IO Bool
checkSort logFile = withSystemTempDirectory "check-sort" $ \tmpDir -> do
  Right eventlog <- readEventLogFromFile logFile
  Sort.sortEvents' sortParams tmpDir (tmpDir </> "out") eventlog
  let inMem = sortEvents $ events $ dat eventlog
  Right merged <- readEventLogFromFile (tmpDir </> "out")
  if show (events $ dat merged) == show inMem
    then return True
    else putStrLn "bad" >> return False

main :: IO ()
main = do
  successes <- mapM checkSort files
  unless (and successes) exitFailure
