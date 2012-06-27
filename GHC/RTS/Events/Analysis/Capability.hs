module GHC.RTS.Events.Analysis.Capability
  ( capabilityThreadPoolMachine
  , capabilityThreadRunMachine
  , capabilityThreadIndexer
  , capabilityTaskPoolMachine
  , capabilityTaskOSMachine
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
    -- TODO: take into account paused threads
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

capabilityThreadIndexer :: Map Int ThreadId -> CapEvent -> Maybe ThreadId
capabilityThreadIndexer m capEvent = case spec . ce_event $ capEvent of
  (CreateSparkThread threadId)  -> Just threadId
  (CreateThread threadId)       -> Just threadId
  (RunThread threadId)          -> Just threadId
  (StopThread threadId _)       -> Just threadId
  (ThreadRunnable threadId)     -> Just threadId
  (MigrateThread threadId _)    -> Just threadId
  (WakeupThread threadId capId) -> if Just capId == ce_cap capEvent
                                   then Just threadId
                                   else Nothing
  _                             -> mThreadId
 where
  mThreadId = ce_cap capEvent >>= (\capId -> M.lookup capId m)

-- | This state machine tracks Haskell tasks, represented by Pthread_t,
-- residing on capabilities.
-- Each Haskell task can only reside on one capability, but can be migrated
-- between them.
capabilityTaskPoolMachine :: Machine (Map Pthread_t Int) CapEvent
capabilityTaskPoolMachine = Machine
  { initial = M.empty
  , final   = const False
  , alpha   = capabilityTaskPoolMachineAlpha
  , delta   = capabilityTaskPoolMachineDelta
  }
 where
  capabilityTaskPoolMachineAlpha capEvent = case spec . ce_event $ capEvent of
     TaskCreate{}  -> True
     TaskDelete{}  -> True
     TaskMigrate{} -> True
     _             -> False

  capabilityTaskPoolMachineDelta mapping capEvent = do
    case spec . ce_event $ capEvent of
      TaskCreate {taskID, cap}      -> insertTask taskID cap mapping
      TaskDelete {taskID}           -> deleteTask taskID mapping
      TaskMigrate {taskID, new_cap} -> deleteTask taskID mapping >>=
                                         insertTask taskID new_cap
      _                             -> Nothing
   where
    insertTask :: Pthread_t -> Int -> Map Pthread_t Int
               -> Maybe (Map Pthread_t Int)
    insertTask taskID cap m
      | taskID `elem` M.keys m = Nothing -- The task already exists.
      | otherwise              = Just $ M.insert taskID cap m

    deleteTask :: Pthread_t -> Map Pthread_t Int -> Maybe (Map Pthread_t Int)
    deleteTask taskID m
      | notElem taskID . M.keys $ m = Nothing -- The task doesn't exist.
      | otherwise                   = Just $ M.delete taskID m

-- | This state machine tracks Haskell tasks (represented by the OS_TID
-- of their OS thread) residing on capabilities and additionally
-- tracks the (immutable) assignment of OS thread ids (OS_TID)
-- to tasks ids (Pthread_t).
-- Each Haskell task can only reside on one capability, but can be migrated
-- between them.
--
-- Data invariat for the @(Map OS_TID Int, Map Pthread_t OS_TID)@ type:
-- the second map is an injection (verified by the machine in insertTaskOS) and
-- the following sets are equal: keys of the fist map and values of the second
-- (follows from the construction of the maps by the machine).
--
-- The machine verifies as much as capabilityTaskPoolMachine and additionally
-- the data invariant, and offers a richer verification profile.
capabilityTaskOSMachine :: Machine (Map OS_TID Int, Map Pthread_t OS_TID)
                                   CapEvent
capabilityTaskOSMachine = Machine
  { initial = (M.empty, M.empty)
  , final   = const False
  , alpha   = capabilityTaskOSMachineAlpha
  , delta   = capabilityTaskOSMachineDelta
  }
 where
  capabilityTaskOSMachineAlpha capEvent = case spec . ce_event $ capEvent of
     TaskCreate{}  -> True
     TaskDelete{}  -> True
     TaskMigrate{} -> True
     _             -> False

  capabilityTaskOSMachineDelta mapping capEvent = do
    case spec . ce_event $ capEvent of
      TaskCreate {taskID, cap, tid} -> insertTaskOS taskID cap tid mapping
      TaskDelete {taskID}           -> deleteTaskOS taskID mapping
      TaskMigrate {taskID, new_cap} -> migrateTaskOS taskID new_cap mapping
      _                             -> Nothing
   where
    insertTaskOS :: Pthread_t -> Int -> OS_TID
                 -> (Map OS_TID Int, Map Pthread_t OS_TID)
                 -> Maybe (Map OS_TID Int, Map Pthread_t OS_TID)
    insertTaskOS taskID cap tid (m, ma)
      | taskID `elem` M.keys ma = Nothing  -- The task already exists.
      | tid `elem` M.keys m     = Nothing  -- The OS thread already exists.
      | otherwise               = Just (M.insert tid cap m,
                                        M.insert taskID tid ma)

    deleteTaskOS :: Pthread_t -> (Map OS_TID Int, Map Pthread_t OS_TID)
                 -> Maybe (Map OS_TID Int, Map Pthread_t OS_TID)
    deleteTaskOS taskID (m, ma) =
      case M.lookup taskID ma of
        Nothing  -> Nothing  -- The task doesn't exist.
        Just tid -> Just (M.delete tid m,
                          M.delete taskID ma)

    migrateTaskOS :: Pthread_t -> Int -> (Map OS_TID Int, Map Pthread_t OS_TID)
                  -> Maybe (Map OS_TID Int, Map Pthread_t OS_TID)
    migrateTaskOS taskID new_cap (m, ma) =
      case M.lookup taskID ma of
        Nothing -> Nothing  -- The task doesn't exist.
        Just tid -> Just (M.insert tid new_cap m,
                          ma)  -- The assignment is immutable.
