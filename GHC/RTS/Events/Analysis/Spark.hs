module GHC.RTS.Events.Analysis.Spark
  ( SparkState
  , sparkMachine
  , sparkThreadMachine
  , sparkIndexer
  )
 where

import GHC.RTS.Events
import GHC.RTS.Events.Analysis

import Data.Map (Map)
import qualified Data.Map as M

-- TODO:
-- The spark is in a finish state when the thread has finished (but not
-- yielding or overflowing etc),
-- or when a new spark is run or stolen.
-- Note that the spark run/stolen instance is also an initial state,
-- so initial states are also final states!

--------------------------------------------------------------------------------
data SparkState
  = SparkInitial  -- ^ Spark initial state
  | SparkRunning  -- ^ Spark converted and running
  | SparkPaused   -- ^ Spark paused
  | SparkFinal    -- ^ Spark final state
  deriving (Eq, Ord, Show)

-- | This state machine tracks the progress of a single spark after it
-- has been converted.
sparkMachine :: Machine SparkState EventInfo
sparkMachine = Machine
  { initial = SparkInitial
  , final   = sparkFinal
  , alpha   = sparkAlpha
  , delta   = sparkDelta
  }
 where
  sparkFinal SparkFinal = True
  sparkFinal _          = False

  sparkAlpha (RunThread _)    = True
  sparkAlpha (StopThread _ _) = True
  -- sparkAlpha SparkCreate    = True
  -- sparkAlpha SparkDud       = True
  -- sparkAlpha SparkOverflow  = True
  sparkAlpha SparkRun       = True
  sparkAlpha (SparkSteal _) = True
  -- sparkAlpha SparkFizzle    = True
  -- sparkAlpha SparkGC        = True
  sparkAlpha _              = False

  -- SparkInitial
  sparkDelta SparkInitial SparkRun       = Just SparkRunning
  sparkDelta SparkInitial (SparkSteal _) = Just SparkRunning
  -- SparkRunning
  sparkDelta SparkRunning SparkRun                      = Just SparkFinal
  sparkDelta SparkRunning (SparkSteal _)                = Just SparkFinal
  sparkDelta SparkRunning (StopThread _ ThreadFinished) = Just SparkFinal
  sparkDelta SparkRunning (StopThread _ _)              = Just SparkPaused
  -- SparkPaused
  sparkDelta SparkPaused (RunThread _) = Just SparkRunning
  -- Otherwise
  sparkDelta _ _ = Nothing

-- | This machine maps capabilities to spark threads
sparkThreadMachine :: Machine (Map Int ThreadId) CapEvent
sparkThreadMachine = Machine
  { initial = M.empty
  , final   = const False
  , alpha   = sparkThreadAlpha
  , delta   = sparkThreadDelta
  }
 where
  sparkThreadAlpha capEvent = case spec . ce_event $ capEvent of
    (CreateSparkThread _) -> True
    (MigrateThread _ _)   -> True
    (StopThread _ _)      -> True
    _                     -> False

  -- TODO: take into account paused threads
  -- This might mean that some stops are just pauses, synced by runs, or
  -- maybe the non-critical stops are just ignored?
  sparkThreadDelta mapping capEvent = do
    capId <- ce_cap capEvent
    case spec . ce_event $ capEvent of
      (CreateSparkThread threadId)         -> createThread capId threadId mapping
      (MigrateThread threadId capId')      -> migrateThread capId capId' threadId mapping
      (StopThread threadId ThreadFinished) -> stopThread threadId mapping
      _                                    -> Just mapping
   where
    createThread :: Int -> ThreadId -> Map Int ThreadId -> Maybe (Map Int ThreadId)
    createThread capId threadId m
      | M.member capId m          = Nothing -- A thread is already on this cap
      | threadId `elem` M.elems m = Nothing -- This thread is already on a cap
      | otherwise                 = Just $ M.insert capId threadId m
    -- The migration must be conservative: we only perform migrations on caps
    -- that are known to have spark threads already.
    -- If there is no mapping we return Just, since migrations might be for ordinary threads.
    migrateThread :: Int -> Int -> ThreadId -> Map Int ThreadId -> Maybe (Map Int ThreadId)
    migrateThread capId capId' threadId m
      | M.member capId m && not (M.member capId' m) = Just $ M.insert capId' threadId (M.delete capId m)
      | otherwise                                   = Just m
    -- Stopping threads is conservative in the sense that if a stop has been
    -- requested of an ordinary thread, then we should still return the map.
    stopThread :: ThreadId -> Map Int ThreadId -> Maybe (Map Int ThreadId)
    stopThread threadId m
      | notElem threadId . M.elems $ m = Just m -- The spark thread isn't here, but an ordinary thread was stopped
      | otherwise                      = Just $ M.filter (/= threadId) m

-- | A sparkIndexer takes a mapping between caps and threads running sparks,
-- such as that provided by the sparkThreadMachine state.
sparkIndexer :: Map Int ThreadId -> CapEvent -> Maybe ThreadId
sparkIndexer mapping capEvent = case spec . ce_event $ capEvent of
  (CreateSparkThread threadId) -> Just threadId
  (CreateThread threadId)      -> isSparkThread threadId
  (RunThread threadId)         -> isSparkThread threadId
  (StopThread threadId _)      -> isSparkThread threadId
  (ThreadRunnable threadId)    -> isSparkThread threadId
  (MigrateThread threadId _)   -> isSparkThread threadId
  (WakeupThread threadId _)    -> isSparkThread threadId
  _                            -> mThreadId
 where
  mThreadId = ce_cap capEvent >>= (\capId -> M.lookup capId mapping)

  isSparkThread :: ThreadId -> Maybe ThreadId
  isSparkThread threadId
    | threadId `elem` M.elems mapping = Just threadId
    | otherwise                       = Nothing

