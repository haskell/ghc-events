{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module GHC.RTS.EventTypes where

import Data.Word (Word16, Word32, Word64)

{- Type synonyms. -}
type Filename = String

-- EventType.
type EventTypeNum = Word16
type EventTypeDescLen = Word32
type EventTypeDesc = String
type EventTypeSize = Word16
-- Event.
type EventDescription = String
type Timestamp = Word64
type ThreadId = Word32
type CapNo = Word16
type Marker = Word32
type BlockSize = Word32
type RawThreadStopStatus = Word16
type StringId = Word32

-- These types are used by Mercury events.
type ParConjDynId = Word64
type ParConjStaticId = StringId
type SparkId = Word32
type FutureId = Word64

sz_event_type_num :: EventTypeSize
sz_event_type_num = 2
sz_cap :: EventTypeSize
sz_cap  = 2
sz_time :: EventTypeSize
sz_time = 8
sz_tid :: EventTypeSize
sz_tid  = 4
sz_old_tid :: EventTypeSize
sz_old_tid  = 8 -- GHC 6.12 was using 8 for ThreadID when declaring the size
                -- of events, but was actually using 32 bits for ThreadIDs
sz_capset :: EventTypeSize
sz_capset = 4
sz_capset_type :: EventTypeSize
sz_capset_type = 2
sz_block_size :: EventTypeSize
sz_block_size = 4
sz_block_event :: EventTypeSize
sz_block_event = fromIntegral (sz_event_type_num + sz_time + sz_block_size
    + sz_time + sz_cap)
sz_pid :: EventTypeSize
sz_pid = 4
sz_th_stop_status :: EventTypeSize
sz_th_stop_status = 2
sz_string_id :: EventTypeSize
sz_string_id = 4

-- Sizes for Mercury event fields.
sz_par_conj_dyn_id :: EventTypeSize
sz_par_conj_dyn_id = 8
sz_par_conj_static_id :: EventTypeSize
sz_par_conj_static_id = sz_string_id
sz_spark_id :: EventTypeSize
sz_spark_id = 4
sz_future_id :: EventTypeSize
sz_future_id = 8

{-
 - Data type delcarations to build the GHC RTS data format,
 - which is a (header, data) pair.
 -
 - Header contains EventTypes.
 - Data contains Events.
 -}
data EventLog =
  EventLog {
    header :: Header,
    dat    :: Data
  } deriving Show

newtype Header = Header {
     eventTypes :: [EventType]
  } deriving (Show, Eq)

data Data = Data {
     events :: [Event]
  } deriving Show

data EventType =
  EventType {
    num  :: EventTypeNum,
    desc :: EventTypeDesc,
    size :: Maybe EventTypeSize -- ^ 'Nothing' indicates variable size
  } deriving (Show, Eq)

data Event =
  Event {
    time :: {-# UNPACK #-}!Timestamp,
    spec :: EventInfo
  } deriving Show

data EventInfo
  = EventBlock         { end_time     :: Timestamp,
                         cap          :: Int,
                         block_events :: [Event]
                       }
  | Startup            { n_caps :: Int
                       }
  | CreateThread       { thread :: {-# UNPACK #-}!ThreadId
                       }
  | RunThread          { thread :: {-# UNPACK #-}!ThreadId
                       }
  | StopThread         { thread :: {-# UNPACK #-}!ThreadId,
                         status :: ThreadStopStatus
                       }
  | ThreadRunnable     { thread :: {-# UNPACK #-}!ThreadId
                       }
  | MigrateThread      { thread :: {-# UNPACK #-}!ThreadId,
                         newCap :: {-# UNPACK #-}!Int
                       }
                       }
  | CreateSparkThread  { sparkThread :: {-# UNPACK #-}!ThreadId
                       }
  | SparkCounters      { sparksCreated, sparksDud, sparksOverflowed,
                         sparksConverted, sparksFizzled, sparksGCd,
                         sparksRemaining :: {-# UNPACK #-}! Word64
                       }
  | WakeupThread       { thread :: {-# UNPACK #-}!ThreadId,
                         otherCap :: {-# UNPACK #-}!Int
                       }
  | Shutdown           { }
  | SparkCreate        { }
  | SparkDud           { }
  | SparkOverflow      { }
  | SparkRun           { }
  | SparkSteal         { victimCap :: {-# UNPACK #-}!Int }
  | SparkFizzle        { }
  | SparkGC            { }
  | RequestSeqGC       { }
  | RequestParGC       { }
  | StartGC            { }
  | GCWork             { }
  | GCIdle             { }
  | GCDone             { }
  | EndGC              { }
  | CapsetCreate       { capset     :: {-# UNPACK #-}!Word32
                       , capsetType :: CapsetType
                       }
  | CapsetDelete       { capset :: {-# UNPACK #-}!Word32
                       }
  | CapsetAssignCap    { capset :: {-# UNPACK #-}!Word32
                       , cap    :: {-# UNPACK #-}!Int
                       }
  | CapsetRemoveCap    { capset :: {-# UNPACK #-}!Word32
                       , cap    :: {-# UNPACK #-}!Int
                       }
  | RtsIdentifier      { capset :: {-# UNPACK #-}!Word32
                       , rtsident :: String
                       }
  | ProgramArgs        { capset :: {-# UNPACK #-}!Word32
                       , args   :: [String]
                       }
  | ProgramEnv         { capset :: {-# UNPACK #-}!Word32
                       , env    :: [String]
                       }
  | OsProcessPid       { capset :: {-# UNPACK #-}!Word32
                       , pid    :: {-# UNPACK #-}!Word32
                       }
  | OsProcessParentPid { capset :: {-# UNPACK #-}!Word32
                       , ppid   :: {-# UNPACK #-}!Word32
                       }
  | Message            { msg :: String }
  | UserMessage        { msg :: String }
  | UnknownEvent       { ref  :: {-# UNPACK #-}!EventTypeNum }

  -- These events have been added for Mercury's benifit but are generally
  -- useful.
  | String             { str :: String, id :: {-# UNPACK #-}!StringId }

  -- Mercury specific events.
  | MerStartParConjunction {
        dyn_id      :: {-# UNPACK #-}!ParConjDynId,
        static_id   :: {-# UNPACK #-}!ParConjStaticId
    }
  | MerEndParConjunction {
        dyn_id      :: {-# UNPACK #-}!ParConjDynId
    }
  | MerEndParConjunct {
        dyn_id      :: {-# UNPACK #-}!ParConjDynId
    }
  | MerCreateSpark {
        dyn_id      :: {-# UNPACK #-}!ParConjDynId,
        spark_id    :: {-# UNPACK #-}!SparkId
    }
  | MerFutureCreate {
        future_id   :: {-# UNPACK #-}!FutureId,
        name_id     :: {-# UNPACK #-}!StringId
    }
  | MerFutureWaitNosuspend {
        future_id   :: {-# UNPACK #-}!FutureId
    }
  | MerFutureWaitSuspended {
        future_id   :: {-# UNPACK #-}!FutureId
    }
  | MerFutureSignal {
        future_id   :: {-# UNPACK #-}!FutureId
    }
  | MerLookingForGlobalThread
  | MerWorkStealing
  | MerLookingForLocalSpark
  | MerReleaseThread {
        thread_id   :: {-# UNPACK #-}!ThreadId
    }
  | MerCapSleeping
  | MerCallingMain

  deriving Show

--sync with ghc/includes/Constants.h
data ThreadStopStatus
 = NoStatus
 | HeapOverflow
 | StackOverflow
 | ThreadYielding
 | ThreadBlocked
 | ThreadFinished
 | ForeignCall
 | BlockedOnMVar
 | BlockedOnBlackHole
 | BlockedOnRead
 | BlockedOnWrite
 | BlockedOnDelay
 | BlockedOnSTM
 | BlockedOnDoProc
 | BlockedOnCCall
 | BlockedOnCCall_NoUnblockExc
 | BlockedOnMsgThrowTo
 | ThreadMigrating
 | BlockedOnMsgGlobalise
 | BlockedOnBlackHoleOwnedBy {-# UNPACK #-}!ThreadId
 deriving (Show)

mkStopStatus :: RawThreadStopStatus -> ThreadStopStatus
mkStopStatus n = case n of
 0  ->  NoStatus
 1  ->  HeapOverflow
 2  ->  StackOverflow
 3  ->  ThreadYielding
 4  ->  ThreadBlocked
 5  ->  ThreadFinished
 6  ->  ForeignCall
 7  ->  BlockedOnMVar
 8  ->  BlockedOnBlackHole
 9  ->  BlockedOnRead
 10 ->  BlockedOnWrite
 11 ->  BlockedOnDelay
 12 ->  BlockedOnSTM
 13 ->  BlockedOnDoProc
 14 ->  BlockedOnCCall
 15 ->  BlockedOnCCall_NoUnblockExc
 16 ->  BlockedOnMsgThrowTo
 17 ->  ThreadMigrating
 18 ->  BlockedOnMsgGlobalise
 _  ->  error "mkStat"

maxThreadStopStatus :: RawThreadStopStatus
maxThreadStopStatus = 18

data CapsetType
 = CapsetCustom
 | CapsetOsProcess
 | CapsetClockDomain
 | CapsetUnknown
 deriving Show

mkCapsetType :: Word16 -> CapsetType
mkCapsetType n = case n of
 1 -> CapsetCustom
 2 -> CapsetOsProcess
 3 -> CapsetClockDomain
 _ -> CapsetUnknown

-- | An event annotated with the Capability that generated it, if any
data CapEvent
  = CapEvent { ce_cap   :: Maybe Int,
               ce_event :: Event
               -- we could UNPACK ce_event, but the Event constructor
               -- might be shared, in which case we could end up
               -- increasing the space usage.
             }

