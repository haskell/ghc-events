{-# LANGUAGE CPP #-}

module Main where

import GHC.RTS.EventsInternal
import GHC.RTS.Events
import GHC.RTS.Events.Merge
import GHC.RTS.Events.Analysis
import GHC.RTS.Events.Analysis.SparkThread
import GHC.RTS.Events.Analysis.Thread
import GHC.RTS.Events.Analysis.Capability

import System.Environment
import Control.Concurrent (threadDelay, forkFinally)
import Control.Monad (forever)
import Data.Either (rights)
import Data.Map (Map)
import qualified Data.Map as M
import Debug.Trace (trace)
import Network
import System.IO
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 709
import System.Exit hiding (die)
#else
import System.Exit
#endif
import Text.Printf

main :: IO ()
main = getArgs >>= command

port :: Int
port = 44444

pullEvents :: Handle -> IO ()
pullEvents h = do
    eh <- ehOpen h 4096
    -- Using True so that the handle would be queried until the log is complete
    trace "Pulling events" $ printEventsIncremental eh True

command :: [String] -> IO ()
command ["--help"] = putStr usage

command ["inc", file] = do
    h <- openBinaryFile file ReadMode
    eh <- ehOpen h 4096
    printEventsIncremental eh False

command ["exp"] = withSocketsDo $ do
  sock <- listenOn (PortNumber (fromIntegral port))
  printf "Listening on port %d\n" port
  forever $ do
    (handle, host, port) <- accept sock
    printf "Accepted connection from %s: %s\n" host (show port)
    forkFinally (pullEvents handle)
                (\_ -> putStrLn "closed" >> hClose handle)

command ["inc", "force", file] = do
    h <- openBinaryFile file ReadMode
    eh <- ehOpen h 1024
    printEventsIncremental eh True

command ["show", file] = do
    log <- readLogOrDie file
    putStrLn $ ppEventLog log

command ["show", "threads", file] = do
    eventLog <- readLogOrDie file
    let eventTypeMap = buildEventTypeMap . eventTypes . header $ eventLog
        evts = sortEvents $ events $ dat eventLog
        mappings  = rights . validates capabilityThreadRunMachine $ evts
        indexes = zipWith capabilityThreadIndexer mappings evts
        threadMap = M.fromListWith (++) . reverse $ zip indexes (map return evts)
    putStrLn "Event Types:"
    putStrLn . unlines . map ppEventType . eventTypes . header $ eventLog
    putStrLn "Thread Indexed Events:"
    putStrLn . showMap
      ((++ "\n") . show)
      (unlines . map (("  " ++) . ppEvent eventTypeMap)) $
        threadMap

command ["show", "caps", file] = do
    eventLog <- readLogOrDie file
    let eventTypeMap = buildEventTypeMap . eventTypes . header $ eventLog
    let evts = sortEvents . events . dat $ eventLog
        indexes = map evCap evts
        capMap = M.fromListWith (++) . reverse $ zip indexes (map return evts)
    putStrLn "Event Types:"
    putStrLn . unlines . map ppEventType . eventTypes . header $ eventLog
    putStrLn "Cap Indexed Events:"
    putStrLn . showMap
      ((++ "\n") . show)
      (unlines . map (("  " ++) . ppEvent eventTypeMap)) $
        capMap

command ["merge", out, file1, file2] = do
    log1 <- readLogOrDie file1
    log2 <- readLogOrDie file2
    let m = mergeEventLogs log1 log2
    writeEventLogToFile out m

command ["validate", "threads", file] = do
    eventLog <- readLogOrDie file
    let evts = sortEvents . events . dat $ eventLog
    let result = validate (routeM capabilityThreadRunMachine
                                  capabilityThreadIndexer
                                  (refineM evSpec threadMachine))
                          evts
    putStrLn $ showValidate (\(m, n) ->
                               "\nThread States:\n" ++ showIndexed show show m ++
                               "\nCap States:\n" ++ showIndexed show show n)
                            show result

command ["validate", "threadpool", file] = do
    eventLog <- readLogOrDie file
    let evts = sortEvents . events . dat $ eventLog
    let result = validate capabilityThreadPoolMachine evts
    putStrLn $ showValidate show show result

command ["validate", "threadrun", file] = do
    eventLog <- readLogOrDie file
    let evts = sortEvents . events . dat $ eventLog
    let result = validate capabilityThreadRunMachine evts
    putStrLn $ showValidate show show result

command ["validate", "taskpool", file] = do
    eventLog <- readLogOrDie file
    let evts = sortEvents . events . dat $ eventLog
    let result = validate capabilityTaskPoolMachine evts
    putStrLn $ showValidate show show result

command ["validate", "tasks", file] = do
    eventLog <- readLogOrDie file
    let evts = sortEvents . events . dat $ eventLog
    let result = validate capabilityTaskOSMachine evts
    putStrLn $ showValidate show show result

command ["validate", "sparks", file] = do
    eventLog <- readLogOrDie file
    let evts = sortEvents . events . dat $ eventLog
    let result = validate
                   (routeM capabilitySparkThreadMachine capabilitySparkThreadIndexer
                     (refineM evSpec sparkThreadMachine))
                   evts
    putStrLn $ showValidate show show result

command ["simulate", "threads", file] = do
    eventLog <- readLogOrDie file
    let evts = sortEvents . events . dat $ eventLog
    let result = simulate (routeM capabilityThreadRunMachine
                                  capabilityThreadIndexer
                                  (refineM evSpec threadMachine))
                          evts
    putStrLn . showProcess $ result

command ["simulate", "threadpool", file] = do
    eventLog <- readLogOrDie file
    let evts = sortEvents . events . dat $ eventLog
    let result = simulate capabilityThreadPoolMachine evts
    putStrLn . showProcess $ result

command ["simulate", "threadrun", file] = do
    eventLog <- readLogOrDie file
    let evts = sortEvents . events . dat $ eventLog
    let result = simulate capabilityThreadRunMachine evts
    putStrLn . showProcess $ result

command ["simulate", "taskpool", file] = do
    eventLog <- readLogOrDie file
    let evts = sortEvents . events . dat $ eventLog
    let result = simulate capabilityTaskPoolMachine evts
    putStrLn . showProcess $ result

command ["simulate", "tasks", file] = do
    eventLog <- readLogOrDie file
    let evts = sortEvents . events . dat $ eventLog
    let result = simulate capabilityTaskOSMachine evts
    putStrLn . showProcess $ result

command ["simulate", "sparks", file] = do
    eventLog <- readLogOrDie file
    let evts = sortEvents . events . dat $ eventLog
    let result = simulate
              (routeM capabilitySparkThreadMachine
                  capabilitySparkThreadIndexer
                  (refineM evSpec sparkThreadMachine)) evts
    putStrLn . showProcess $ result

command ["profile", "threads", file] = do
    eventLog <- readLogOrDie file
    let evts = sortEvents . events . dat $ eventLog
    let result = profileRouted
                   (refineM evSpec threadMachine)
                   capabilityThreadRunMachine
                   capabilityThreadIndexer evTime evts
    putStrLn . showProcess $ result

command ["profile", "sparks", file] = do
    eventLog <- readLogOrDie file
    let evts = sortEvents . events . dat $ eventLog
    let result = profileRouted
                   (refineM evSpec sparkThreadMachine)
                   capabilitySparkThreadMachine
                   capabilitySparkThreadIndexer
                   evTime evts
    putStrLn . showProcess $ result

command _ = putStr usage >> die "Unrecognized command"
-- command (x:xs) = putStr x >> command xs
-- command _ = putStr usage >> die "Unrecognized command"

die s = do hPutStrLn stderr s; exitWith (ExitFailure 1)

readLogOrDie :: FilePath -> IO EventLog
readLogOrDie file = do
    e <- readEventLogFromFile file
    case e of
        Left s    -> die ("Failed to parse " ++ file ++ ": " ++ s)
        Right log -> return log

usage = unlines $ map pad strings
 where
    align = 4 + (maximum . map (length . fst) $ strings)
    pad (x, y) = zipWith const (x ++ repeat ' ') (replicate align ()) ++ y
    strings = [ ("ghc-events --help:",                     "Display this help.")
              , ("ghc-events inc <file>:",                 "Pretty print an event log incrementally")
              , ("ghc-events inc force <file>:",           "Pretty print an event log incrementally. Retry on incomplete input (aka 'tail -f').")
              , ("ghc-events show <file>:",                "Pretty print an event log.")
              , ("ghc-events show threads <file>:",        "Pretty print an event log, ordered by threads.")
              , ("ghc-events show caps <file>:",           "Pretty print an event log, ordered by capabilities.")

              , ("ghc-events merge <out> <in1> <in2>:",    "Merge two event logs.")

              , ("ghc-events sparks-csv <file>:",          "Print spark information in CSV.")

              , ("ghc-events validate threads <file>:",    "Validate thread states.")
              , ("ghc-events validate threadpool <file>:", "Validate thread pool state.")
              , ("ghc-events validate threadrun <file>:",  "Validate thread running state.")
              , ("ghc-events validate tasks <file>:",      "Validate task states.")
              , ("ghc-events validate sparks <file>:",     "Validate spark thread states.")

              , ("ghc-events simulate threads <file>:",    "Simulate thread states.")
              , ("ghc-events simulate threadpool <file>:", "Simulate thread pool state.")
              , ("ghc-events simulate threadrun <file>:",  "Simulate thread running state.")
              , ("ghc-events simulate tasks <file>:",      "Simulate task states.")
              , ("ghc-events simulate sparks <file>:",     "Simulate spark thread states.")

              , ("ghc-events profile threads <file>:",     "Profile thread states.")
              , ("ghc-events profile sparks <file>:",      "Profile spark thread states.")
              ]

showValidate :: (s -> String) -> (i -> String) -> Either (s, i) s -> String
showValidate showState showInput (Left (state, input)) =
  "Invalid eventlog:"
  ++ "\nState:\n" ++ showState state
  ++ "\nInput:\n" ++ showInput input
showValidate showState _ (Right state) =
  "Valid eventlog: " ++ showState state

showProcess :: (Show e, Show a) => Process e a -> String
showProcess process =
  "Trace:\n"
  ++ (unlines . map show . toList) process
  ++ "\n"
  ++ (maybe "Valid." (("Invalid:\n" ++) . show) . toMaybe) process

showIndexed :: (k -> String) -> (v -> String) -> Map k v -> String
showIndexed showKey showValue m
  | M.null m  = "Empty map\n"
  | otherwise = "Indexed output:\n" ++
      concatMap (\(k, v) -> "Key: " ++ showKey k ++ ", Value: "
          ++ showValue v ++ "\n")
        (M.toList m)

showMap :: Ord k => (k -> String) -> (a -> String) -> M.Map k a -> String
showMap showKey showValue m =
  concat $ zipWith (++)
    (map showKey . M.keys $ m :: [String])
    (map (showValue . (M.!) m) . M.keys $ m :: [String])

printEventsIncremental :: EventHandle
                       -> Bool -- Whether to retry on incomplete logs
                       -> IO ()
printEventsIncremental eh dashf = do
    event <- ehReadEvent eh
    case event of
      One ev -> do
          putStrLn (ppEvent' ev) -- if actual printing is needed
          printEventsIncremental eh dashf
      PartialEventLog ->
        if dashf
          then print "Log Incomplete. Waiting for more input." >> threadDelay 1000000 >> printEventsIncremental eh dashf
          else putStrLn "Finished (NOT all file was parsed successfully)"
      CompleteEventLog ->
        putStrLn "Finished (file was parsed successfully)"
      EventLogParsingError errMsg ->
        putStrLn $ "Error: " ++ errMsg
