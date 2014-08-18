module GHC.RTS.Events.Analysis.SparkThread
  ( SparkThreadState (..)
  , sparkThreadMachine
  , capabilitySparkThreadMachine
  , capabilitySparkThreadIndexer
  )
 where

import GHC.RTS.Events
import GHC.RTS.Events.Analysis

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import Debug.Trace

data SparkThreadState
  = SparkThreadInitial
  | SparkThreadCreated
  | SparkThreadRunning Int
  | SparkThreadPaused Int
  | SparkThreadFinal
  deriving (Eq, Ord, Show)

sparkThreadMachine :: Machine SparkThreadState EventInfo
sparkThreadMachine = Machine
  { initial = SparkThreadInitial
  , final   = sparkThreadFinal
  , alpha   = sparkThreadAlpha
  , delta   = sparkThreadDelta
  }
 where
  sparkThreadFinal SparkThreadFinal = True
  sparkThreadFinal _                = False

  -- sparkThreadAlpha (CreateSparkThread _) = True
  sparkThreadAlpha (RunThread _)         = True
  sparkThreadAlpha (StopThread _ _)      = True
  sparkThreadAlpha SparkRun              = True
  sparkThreadAlpha (SparkSteal _)        = True
  sparkThreadAlpha _                     = False

  -- SparkThreadInitial
  -- sparkThreadDelta SparkThreadInitial (CreateSparkThread _) = Just SparkThreadInitial
  sparkThreadDelta SparkThreadInitial (RunThread _)         = Just SparkThreadCreated
  -- SparkThreadCreated
  sparkThreadDelta SparkThreadCreated SparkRun                      = Just (SparkThreadRunning 0)
  sparkThreadDelta SparkThreadCreated (SparkSteal _)                = Just (SparkThreadRunning 0)
  sparkThreadDelta SparkThreadCreated (StopThread _ ThreadFinished) = Just SparkThreadFinal
  -- SparkThreadRunning
  sparkThreadDelta (SparkThreadRunning n) (StopThread _ ThreadFinished) = Just SparkThreadFinal
  sparkThreadDelta (SparkThreadRunning n) (StopThread _ _)              = Just (SparkThreadPaused n)
  sparkThreadDelta (SparkThreadRunning n) SparkRun                      = Just (SparkThreadRunning (n+1))
  sparkThreadDelta (SparkThreadRunning n) (SparkSteal _)                = Just (SparkThreadRunning (n+1))
  -- SparkThreadPaused
  sparkThreadDelta (SparkThreadPaused n) (RunThread _) = Just (SparkThreadRunning n)
  -- Other
  sparkThreadDelta _ _ = Nothing

capabilitySparkThreadMachine :: Machine (Map Int ThreadId, Set ThreadId) Event
capabilitySparkThreadMachine = Machine
  { initial = (M.empty, S.empty)
  , final   = const False
  , alpha   = capabilitySparkThreadAlpha
  , delta   = capabilitySparkThreadDelta
  }
 where
  capabilitySparkThreadAlpha evt = case evSpec evt of
    (CreateSparkThread _)         -> True
    (RunThread _)                 -> True
    (StopThread _ _)              -> True
    _                             -> False
  capabilitySparkThreadDelta (m, s) evt = do
    capId <- evCap evt
    case evSpec evt of
      (CreateSparkThread threadId)         -> createThread threadId
      (StopThread threadId _)              -> pauseThread threadId
      (RunThread threadId)                 -> runThread capId threadId
      _                                    -> Just (m, s)
   where
    createThread :: ThreadId -> Maybe (Map Int ThreadId, Set ThreadId)
    createThread threadId
      | S.member threadId s = Nothing -- A spark thread with this Id already created
      | otherwise           = Just (m, S.insert threadId s)

    runThread :: Int -> ThreadId -> Maybe (Map Int ThreadId, Set ThreadId)
    runThread capId threadId
      | M.member capId m          = Nothing      -- A thread is already on this cap
      | threadId `elem` M.elems m = Nothing      -- This thread is already on a cap
      | S.notMember threadId s    = Just (m, s)  -- Not a spark thread
      | otherwise                 = Just (M.insert capId threadId m, S.insert threadId s)

    stopThread :: ThreadId -> Maybe (Map Int ThreadId, Set ThreadId)
    stopThread threadId = Just (M.filter (/= threadId) m, S.delete threadId s)

    pauseThread :: ThreadId -> Maybe (Map Int ThreadId, Set ThreadId)
    pauseThread threadId = Just (M.filter (/= threadId) m, s)

capabilitySparkThreadIndexer :: (Map Int ThreadId, Set ThreadId) -> Event -> Maybe ThreadId
capabilitySparkThreadIndexer (m, s) evt = case evSpec evt of
  (CreateThread threadId)   -> inSparkThreadPool threadId
  (RunThread threadId)      -> inSparkThreadPool threadId
  (StopThread threadId _)   -> inSparkThreadPool threadId
  _                         -> evCap evt >>= (\capId -> M.lookup capId m)
 where
  inSparkThreadPool :: ThreadId -> Maybe ThreadId
  inSparkThreadPool threadId
    | S.member threadId s = Just threadId
    | otherwise           = Nothing

