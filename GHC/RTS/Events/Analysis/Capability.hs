module GHC.RTS.Events.Analysis.Capability
  ( capabilityThreadPoolMachine
  , capabilityThreadRunMachine
  )
 where

import GHC.RTS.Events
import GHC.RTS.Events.Analysis

import Data.Map (Map)
import qualified Data.Map as M

-- | This state machine tracks threads residing on capabilities.
-- Each thread can only reside on one capability, but can be migrated between
-- them.
capabilityThreadPoolMachine :: Machine (Map ThreadId Int) CapEvent
capabilityThreadPoolMachine = Machine
  { initial = M.empty
  , final   = const False
  , alpha   = capabilityThreadPoolMachineAlpha
  , delta   = capabilityThreadPoolMachineDelta
  }
 where
  capabilityThreadPoolMachineAlpha capEvent = case spec . ce_event $ capEvent of
     (CreateThread _)    -> True
     (StopThread _ _)    -> True
     (MigrateThread _ _) -> True
     _                   -> False

  capabilityThreadPoolMachineDelta mapping capEvent = do
    capId <- ce_cap capEvent
    case spec . ce_event $ capEvent of
      (CreateThread threadId)              -> insertThread threadId capId mapping
      (StopThread threadId ThreadFinished) -> deleteThread threadId mapping
      (StopThread _ _)                     -> Just mapping
      (MigrateThread threadId capId')      -> deleteThread threadId mapping >>=
                                                insertThread threadId capId'
      _                                    -> Nothing
   where
    insertThread :: ThreadId -> Int -> Map ThreadId Int -> Maybe (Map ThreadId Int)
    insertThread threadId capId m
      | threadId `elem` M.keys m = Nothing -- The thread already exists
      | otherwise                = Just $ M.insert threadId capId m

    deleteThread :: ThreadId -> Map ThreadId Int -> Maybe (Map ThreadId Int)
    deleteThread threadId m
      | notElem threadId . M.keys $ m = Nothing -- The thread doesn't exist
      | otherwise                     = Just $ M.delete threadId m

-- | This state machine tracks threads running on capabilities, only one thread
-- may run on a capability at a time.
capabilityThreadRunMachine :: Machine (Map Int ThreadId) CapEvent
capabilityThreadRunMachine = Machine
  { initial = M.empty
  , final   = const False
  , alpha   = threadRunAlpha
  , delta   = threadRunDelta
  }
 where
  threadRunAlpha capEvent = case spec . ce_event $ capEvent of
    -- TODO: can threads be migrated while they are running?
    (RunThread _)     -> True
    (StopThread _ _ ) -> True
    _                 -> False

  -- The indexer fails if a thread is inserted where one already exists,
  -- or if a thread is deleted that doesn't exist.
  threadRunDelta mapping e = do
    capId <- ce_cap e
    case spec . ce_event $ e of
      (RunThread threadId)     -> runThread capId threadId mapping
      (StopThread threadId _ ) -> stopThread threadId mapping
      _                        -> Just mapping
   where
    runThread :: Int -> ThreadId -> Map Int ThreadId -> Maybe (Map Int ThreadId)
    runThread capId threadId m
      | M.member capId m          = Nothing -- A thread is already on this cap
      | threadId `elem` M.elems m = Nothing -- This thread is already on a cap
      | otherwise                 = Just $ M.insert capId threadId m
    stopThread :: ThreadId -> Map Int ThreadId -> Maybe (Map Int ThreadId)
    stopThread threadId m
      | notElem threadId . M.elems $ m = Nothing -- The thread doesn't exist
      | otherwise                      = Just $ M.filter (/= threadId) m
