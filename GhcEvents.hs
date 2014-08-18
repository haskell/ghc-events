{-# LANGUAGE CPP #-}

module Main where

import GHC.RTS.Events
import GHC.RTS.EventsIncremental
import GHC.RTS.Events.Merge
import GHC.RTS.Events.Analysis
import GHC.RTS.Events.Analysis.SparkThread
import GHC.RTS.Events.Analysis.Thread
import GHC.RTS.Events.Analysis.Capability

import qualified Data.ByteString as B
import System.Environment
import Control.Concurrent (threadDelay)
import Text.Printf
import Data.List
import Data.Either (rights)
import Data.Function
import Data.IntMap (IntMap)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import System.IO
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 709
import System.Exit hiding (die)
#else
import System.Exit
#endif

main = getArgs >>= command

command ["--help"] = do
    putStr usage

command ["inc", file] = do
    h  <- openBinaryFile file ReadMode
    printEventsIncremental Nothing h

command ["show", file] = do
    log <- readEventLogFromFile file
    putStrLn $ ppEventLog log

command ["show", "threads", file] = do
    eventLog <- readEventLogFromFile file
    let eventTypeMap = buildEventTypeMap . eventTypes . header $ eventLog
        evts = sortEvents $ events $ dat eventLog
        mappings  = rights . validates capabilityThreadRunMachine $ evts
        indexes = map (uncurry capabilityThreadIndexer) $ zip mappings evts
        threadMap = M.fromListWith (++) . reverse $ zip indexes (map return evts)
    putStrLn "Event Types:"
    putStrLn . unlines . map ppEventType . eventTypes . header $ eventLog
    putStrLn "Thread Indexed Events:"
    putStrLn . showMap
      ((++ "\n") . show)
      (unlines . map (("  " ++) . ppEvent eventTypeMap)) $
        threadMap

command ["show", "caps", file] = do
    eventLog <- readEventLogFromFile file
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
    log1 <- readEventLogFromFile file1
    log2 <- readEventLogFromFile file2
    let m = mergeEventLogs log1 log2
    writeEventLogToFile out m

command ["validate", "threads", file] = do
    eventLog <- readEventLogFromFile file
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
    eventLog <- readEventLogFromFile file
    let evts = sortEvents . events . dat $ eventLog
    let result = validate capabilityThreadPoolMachine evts
    putStrLn $ showValidate show show result

command ["validate", "threadrun", file] = do
    eventLog <- readEventLogFromFile file
    let evts = sortEvents . events . dat $ eventLog
    let result = validate capabilityThreadRunMachine evts
    putStrLn $ showValidate show show result

command ["validate", "taskpool", file] = do
    eventLog <- readEventLogFromFile file
    let evts = sortEvents . events . dat $ eventLog
    let result = validate capabilityTaskPoolMachine evts
    putStrLn $ showValidate show show result

command ["validate", "tasks", file] = do
    eventLog <- readEventLogFromFile file
    let evts = sortEvents . events . dat $ eventLog
    let result = validate capabilityTaskOSMachine evts
    putStrLn $ showValidate show show result

command ["validate", "sparks", file] = do
    eventLog <- readEventLogFromFile file
    let evts = sortEvents . events . dat $ eventLog
    let result = validate
                   (routeM capabilitySparkThreadMachine capabilitySparkThreadIndexer
                     (refineM evSpec sparkThreadMachine))
                   evts
    putStrLn $ showValidate show show result

command ["simulate", "threads", file] = do
    eventLog <- readEventLogFromFile file
    let evts = sortEvents . events . dat $ eventLog
    let result = simulate (routeM capabilityThreadRunMachine
                                  capabilityThreadIndexer
                                  (refineM evSpec threadMachine))
                          evts
    putStrLn . showProcess $ result

command ["simulate", "threadpool", file] = do
    eventLog <- readEventLogFromFile file
    let evts = sortEvents . events . dat $ eventLog
    let result = simulate capabilityThreadPoolMachine evts
    putStrLn . showProcess $ result

command ["simulate", "threadrun", file] = do
    eventLog <- readEventLogFromFile file
    let evts = sortEvents . events . dat $ eventLog
    let result = simulate capabilityThreadRunMachine evts
    putStrLn . showProcess $ result

command ["simulate", "taskpool", file] = do
    eventLog <- readEventLogFromFile file
    let evts = sortEvents . events . dat $ eventLog
    let result = simulate capabilityTaskPoolMachine evts
    putStrLn . showProcess $ result

command ["simulate", "tasks", file] = do
    eventLog <- readEventLogFromFile file
    let evts = sortEvents . events . dat $ eventLog
    let result = simulate capabilityTaskOSMachine evts
    putStrLn . showProcess $ result

command ["simulate", "sparks", file] = do
    eventLog <- readEventLogFromFile file
    let evts = sortEvents . events . dat $ eventLog
    let result = simulate
              (routeM capabilitySparkThreadMachine
                  capabilitySparkThreadIndexer
                  (refineM evSpec sparkThreadMachine)) evts
    putStrLn . showProcess $ result

command ["profile", "threads", file] = do
    eventLog <- readEventLogFromFile file
    let evts = sortEvents . events . dat $ eventLog
    let result = profileRouted
                   (refineM evSpec threadMachine)
                   capabilityThreadRunMachine
                   capabilityThreadIndexer evTime evts
    putStrLn . showProcess $ result

command ["profile", "sparks", file] = do
    eventLog <- readEventLogFromFile file
    let evts = sortEvents . events . dat $ eventLog
    let result = profileRouted
                   (refineM evSpec sparkThreadMachine)
                   capabilitySparkThreadMachine
                   capabilitySparkThreadIndexer
                   evTime evts
    putStrLn . showProcess $ result

command _ = putStr usage >> die "Unrecognized command"

die s = do hPutStrLn stderr s; exitWith (ExitFailure 1)

usage = unlines $ map pad strings
 where
    align = 4 + (maximum . map (length . fst) $ strings)
    pad (x, y) = zipWith const (x ++ repeat ' ') (replicate align ()) ++ y
    strings = [ ("ghc-events --help:",                     "Display this help.")

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
  ++ "\nState:\n" ++ ( showState state )
  ++ "\nInput:\n" ++ ( showInput input )
showValidate showState _ (Right state) =
  "Valid eventlog: " ++ ( showState state )

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
      concatMap (\(k, v) -> "Key: " ++ ( showKey k ) ++ ", Value: "
          ++ ( showValue v ) ++ "\n")
        (M.toList m)

showMap :: Ord k => (k -> String) -> (a -> String) -> M.Map k a -> String
showMap showKey showValue m =
  concat $ zipWith (++)
    (map showKey . M.keys $ m :: [String])
    (map (showValue . (M.!) m) . M.keys $ m :: [String])

-- Example client for the API
printEventsIncremental :: Maybe EventParserState -> Handle -> IO ()
printEventsIncremental (Just eps) handle = do
    bs <- B.hGetSome handle 1024
    let (resEvent, newState) =   readEvent eps bs
    let dbg = False
        dashf = True
    case resEvent of
      One ev -> do
          putStrLn (ppEvent' ev)
          -- print events one by one
          input <- if dbg then getLine else return ""
          if input == ""
            then printEventsIncremental (Just newState) handle
            else putStrLn "Stopping"
      PartialEventLog -> do
        if dashf
          then do
                print "waiting for input"
                threadDelay 1000000
                printEventsIncremental (Just newState) handle
          else putStrLn "Incomplete but no -f"
      CompleteEventLog -> do
        putStrLn "Done (file was complete)"
      EventLogParsingError errMsg -> do
        putStrLn "An error has occured."
printEventsIncremental (Nothing) handle = do
    printEventsIncremental (Just initEventParser) handle
