{-# OPTIONS_GHC -funbox-strict-fields #-}
{-
 - Author: Spencer Janssen <spencer@well-typed.com>
 -}

module GHC.RTS.Events.Sparks (SparkEvalInfo(..), sparkInfo, sparkDuration) where

import GHC.RTS.Events

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)

-- this might be premature optimization...
import GHC.Exts (build)

data SparkEvalInfo = SparkEvalInfo
                        { startedOnThread :: !ThreadId
                        , startedOnCap, takenFrom :: !Int
                        , timeStarted, timeFinished, timeIdle :: !Timestamp }
 deriving (Show, Read, Ord, Eq)

-- | The amount of active time spent on the spark.
sparkDuration :: SparkEvalInfo -> Timestamp
sparkDuration sp = timeFinished sp - timeStarted sp - timeIdle sp

-- | Generate information about each spark evaluated.  The input must be a
-- complete event log.  
sparkInfo :: [Event] -> [SparkEvalInfo]
sparkInfo = catMaybes . getSparks . labelThreads . sortEvents
 where getSparks = mapAccumL_ sparkMachine Map.empty

-- | A variant of mapAccumL that does not return the accumulator (more
-- efficient).  Subject to fusion.
mapAccumL_ :: (acc -> x -> (acc, y)) -> acc -> [x] -> [y]
mapAccumL_ f a0 xs = build (\c n -> foldr (go c) (const n) xs a0)
 where
    go c x g a = case f a x of (a', y) -> y `c` g a'

type ExtEvent = (Timestamp, Maybe Int, Maybe ThreadId, EventInfo)

threadMachine :: Map Int ThreadId
              -> CapEvent
              -> (Map Int ThreadId, ExtEvent)
threadMachine m (CapEvent (Just cap) (Event ts e)) = (m', (ts, Just cap, Map.lookup cap m, e))
 where
    m' = case e of
            RunThread tid    -> Map.insert cap tid m
            StopThread tid _ -> Map.delete cap m
            _                -> m

threadMachine m (CapEvent Nothing (Event ts e)) = (m, (ts, Nothing, Nothing, e))

labelThreads :: [CapEvent] -> [ExtEvent]
labelThreads = mapAccumL_ threadMachine Map.empty

data SparkThreadState
   = Started
            -- ^ thread started, but not evaluating a spark at the moment
   | Running SparkEvalInfo
            -- ^ thread is currently running and has a spark
   | Paused  SparkEvalInfo Timestamp
            -- ^ thread is not running, but is in the middle of evaluating a
            -- spark.  Second parameter is the time that the thread was
            -- suspended

sparkMachine :: Map ThreadId SparkThreadState
             -> ExtEvent
             -> (Map ThreadId SparkThreadState, Maybe SparkEvalInfo)
sparkMachine m (now, Just cap, mtid, e) = case (mtid, flip Map.lookup m =<< mtid, e) of
    -- 
    (_, _, CreateSparkThread tid) -> (Map.insert tid Started m, Nothing)

    -- Begin to evaluate a new spark.  If a spark was already running on this
    -- thread, emit it.
    (Just tid, Just ts, SparkRun)       -> (sparkRun tid cap, sparkFinish ts)
    (Just tid, Just ts, SparkSteal vic) -> (sparkRun tid vic, sparkFinish ts)

    -- The thread is done, emit the last spark (if any) and remove the thread.
    (_, Just ts, StopThread tid ThreadFinished) -> (Map.delete tid m, sparkFinish ts)

    -- The thread is temporarily paused for some reason
    (_, Just (Running sp), StopThread tid _) -> (sparkPause tid sp, Nothing)

    -- The thread is restarting after a pause
    (_, Just (Paused sp pause), RunThread tid) -> (sparkResume tid sp pause, Nothing)

    -- Ignore all other events
    _ -> (m, Nothing)
 where
    sparkRun tid vic = Map.insert tid (Running $ SparkEvalInfo tid cap vic now now 0) m

    sparkFinish (Running sp) = Just $ sp { timeFinished = now }
    sparkFinish (Paused  sp pause) = Just $ sp { timeFinished = now
                                               , timeIdle = now - pause + timeIdle sp }
    sparkFinish Started = Nothing

    sparkPause tid sp = Map.insert tid (Paused sp now) m

    sparkResume tid sp pause = Map.insert tid (Running sp { timeIdle = now - pause + timeIdle sp }) m

sparkMachine m _ = (m, Nothing)
