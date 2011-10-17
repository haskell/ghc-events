module Main where

import GHC.RTS.Events
import GHC.RTS.Events.Analysis
import GHC.RTS.Events.Analysis.Thread
import GHC.RTS.Events.Analysis.Capability

import System.Environment (getArgs)
import System.IO (stderr, hPutStrLn)
import System.Exit (exitWith, ExitCode (..))

import Control.Monad (forM_)
import Data.Map (Map)
import qualified Data.Map as M

--------------------------------------------------------------------------------
main :: IO ()
main = do
  -- Get command line arguments
  args <- getArgs
  (machine, file) <- case args of
    [file]       -> return ("thread", file)
    [machine, file] -> return (machine, file)
    _            -> hPutStrLn stderr usage >> exitWith (ExitFailure 1)

  -- Read the eventLog
  eventLog <- do
    eEventLog <- readEventLogFromFile file
    case eEventLog of
      Left s -> do
        hPutStrLn stderr ("Failed to parse " ++ file ++ ": " ++ s)
        exitWith (ExitFailure 1)
      Right eventLog -> return eventLog

  -- Validate the log
  putStrLn ("Starting in mode: " ++ machine)
  case machine of
    "thread" -> do
      let eventInfos = map (spec . ce_event) . sortEvents . events . dat $ eventLog
      let result = runIndexed threadIndexer
                     validate
                       threadMachine
                       eventInfos
      putStrLn $ showIndexed show (showValidate show show) result
    "capabilityThreadPool" -> do
      let capEvents = sortEvents . events . dat $ eventLog
      let result = validate
                     capabilityThreadPoolMachine
                     capEvents
      putStrLn $ showValidate show (show . spec . ce_event) result
    "capabilityThreadRun" -> do
      let capEvents = sortEvents . events . dat $ eventLog
      let result = validate
                     capabilityThreadRunMachine
                     capEvents
      putStrLn $ showValidate show (show . spec . ce_event) result
    "profile" -> do
      let capEvents  = sortEvents . events . dat $ eventLog
      let capTimes   = map (time . ce_event) capEvents
      let eventInfos = map (spec . ce_event) capEvents
      let result = runIndexed (threadIndexer . fst)
                     profile
                       (profiledMachine threadMachine)
                       (zip eventInfos capTimes)
      putStrLn $ showIndexed show (showProfile show show) result
    _ -> do
      hPutStrLn stderr ("Mode not recognised: " ++ machine)
      hPutStrLn stderr usage
      exitWith (ExitFailure 1)
 where
  usage :: String
  usage = "usage: MODE? FILE\n where\n  MODE ::= thread | threadRun | capability"

  showValidate showState showInput (Left (state, input)) =
    "Invalid eventlog:\n"
    ++ "* State:\n"
    ++ ( showState state ) ++ "\\n"
    ++ "* Input:\n"
    ++ ( showInput input ) ++ "\\n"
  showValidate showState _ (Right state) =
    "Valid eventlog:\n"
    ++ ( showState state )

  showIndexed :: (k -> String) -> (v -> String) -> Map k v -> String
  showIndexed showKey showValue m =
    "Indexed output:\n" ++
    concatMap
      (\(k, v) ->
        "* Key:\n"
        ++ ( showKey k ) ++ "\n"
        ++ "* Value:\n"
        ++ ( showValue v ) ++ "\n"
      )
      (M.toList m)

  showProfile = showIndexed

