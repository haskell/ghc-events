{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GHC.RTS.EventTypes where
import Control.Monad
import Data.Bits

import Data.Binary
import Data.Text (Text)
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as VU

-- EventType.
type EventTypeNum = Word16
type EventTypeDescLen = Word32
type EventTypeDesc = Text
type EventTypeSize = Word16
-- Event.
type Timestamp = Word64
type ThreadId = Word32
type CapNo = Word16
type Marker = Word32
type BlockSize = Word32
type RawThreadStopStatus = Word16
type StringId = Word32
type Capset   = Word32
type PerfEventTypeNum = Word32
type TaskId = Word64
type PID = Word32

newtype KernelThreadId = KernelThreadId { kernelThreadId :: Word64 }
  deriving (Eq, Ord, Show)
instance Binary KernelThreadId where
  put (KernelThreadId tid) = put tid
  get = fmap KernelThreadId get

-- Types for Parallel-RTS Extension
type ProcessId = Word32
type MachineId = Word16
type PortId = ThreadId
type MessageSize = Word32
type RawMsgTag = Word8

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
sz_taskid :: EventTypeSize
sz_taskid = 8
sz_kernel_tid :: EventTypeSize
sz_kernel_tid = 8
sz_th_stop_status :: EventTypeSize
sz_th_stop_status = 2
sz_string_id :: EventTypeSize
sz_string_id = 4
sz_perf_num :: EventTypeSize
sz_perf_num = 4

-- Sizes for Parallel-RTS event fields
sz_procid, sz_mid, sz_mes, sz_realtime, sz_msgtag :: EventTypeSize
sz_procid  = 4
sz_mid  = 2
sz_mes  = 4
sz_realtime = 8
sz_msgtag  = 1

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
    evTime  :: {-# UNPACK #-}!Timestamp,
    evSpec  :: EventInfo,
    evCap :: Maybe Int
  } deriving Show

{-# DEPRECATED time "The field is now called evTime" #-}
time :: Event -> Timestamp
time = evTime

{-# DEPRECATED spec "The field is now called evSpec" #-}
spec :: Event -> EventInfo
spec = evSpec

data EventInfo

  -- pseudo events
  = EventBlock         { end_time   :: Timestamp,
                         cap        :: Int,
                         block_size :: BlockSize
                       }
  | UnknownEvent       { ref  :: {-# UNPACK #-}!EventTypeNum }

  -- init and shutdown
  | Startup            { n_caps :: Int
                       }
  -- EVENT_SHUTDOWN is replaced by EVENT_CAP_DELETE and GHC 7.6+
  -- no longer generate the event; should be removed at some point
  | Shutdown           { }

  -- thread scheduling
  -- | Marks the creation of a Haskell thread.
  | CreateThread       { thread :: {-# UNPACK #-}!ThreadId
                       }
  -- | The indicated thread has started running.
  | RunThread          { thread :: {-# UNPACK #-}!ThreadId
                       }
  -- | The indicated thread has stopped running for the reason given by status.
  | StopThread         { thread :: {-# UNPACK #-}!ThreadId,
                         status :: !ThreadStopStatus
                       }
  -- | The indicated thread has been marked as ready to run.
  | ThreadRunnable     { thread :: {-# UNPACK #-}!ThreadId
                       }
  -- | The indicated thread has been migrated to a new capability.
  | MigrateThread      { thread :: {-# UNPACK #-}!ThreadId,
                         newCap :: {-# UNPACK #-}!Int
                       }
  -- TODO: Called "THREAD_WAKEUP" in GHC user guide
  -- | The indicated thread has been woken up on another capability.
  | WakeupThread       { thread :: {-# UNPACK #-}!ThreadId,
                         otherCap :: {-# UNPACK #-}!Int
                       }
  -- | The indicated thread has been given a label (e.g. with GHC.Conc.labelThread).
  | ThreadLabel        { thread :: {-# UNPACK #-}!ThreadId,
                         threadlabel :: !Text
                       }

  -- par sparks
  -- | A thread has been created to perform spark evaluation.
  | CreateSparkThread  { sparkThread :: {-# UNPACK #-}!ThreadId
                       }
  -- | A periodic reporting of various statistics of spark evaluation.
  | SparkCounters      { sparksCreated    :: {-# UNPACK #-} !Word64, 
                         sparksDud        :: {-# UNPACK #-} !Word64, 
                         sparksOverflowed :: {-# UNPACK #-} !Word64,
                         sparksConverted  :: {-# UNPACK #-} !Word64, 
                         sparksFizzled    :: {-# UNPACK #-} !Word64, 
                         sparksGCd        :: {-# UNPACK #-} !Word64,
                         sparksRemaining  :: {-# UNPACK #-} !Word64
                       }
  -- | A spark has been added to the spark pool.
  | SparkCreate        { }
  | SparkDud           { }
  | SparkOverflow      { }
  -- | Evaluation has started on a spark.
  | SparkRun           { }
  -- | A spark has been stolen from another capability for evaluation.
  | SparkSteal         { -- | Capability from which the spark was stolen
                         victimCap :: {-# UNPACK #-}!Int }
  -- | A spark has been GC’d before being evaluated.
  | SparkFizzle        { }
  -- | An unevaluated spark has been garbage collected.
  | SparkGC            { }

  -- tasks
  -- | Marks the creation of a task.
  | TaskCreate         { taskId :: TaskId,
                         cap :: {-# UNPACK #-}!Int,
                         -- | The thread-id of the kernel thread which created the task.
                         tid :: {-# UNPACK #-}!KernelThreadId
                       }
  -- | Marks the migration of a task to a new capability.
  | TaskMigrate        { taskId :: TaskId,
                         -- | Old capability
                         cap :: {-# UNPACK #-}!Int,
                         -- | New capability 
                         new_cap :: {-# UNPACK #-}!Int
                       }
  | TaskDelete         { taskId :: TaskId }

  -- garbage collection
  -- | A sequential garbage collection has been requested by a capability.
  | RequestSeqGC       { }
  -- | A parallel garbage collection has been requested by a capability.
  | RequestParGC       { }
  -- TODO: Called GC_START in GHC user guide
  -- | A garbage collection pass has been started.
  | StartGC            { }
  -- | Marks the start of concurrent scavenging.
  | GCWork             { }
  -- | An idle-time garbage collection has been started.
  | GCIdle             { }
  -- | Marks the end of concurrent scavenging.
  | GCDone             { }
  -- TODO: Called GC_END in GHC user guide
  -- | A garbage collection pass has been finished.
  | EndGC              { }
  | GlobalSyncGC       { }
  -- | Report various information about a major collection.
  | GCStatsGHC         { heapCapset   :: {-# UNPACK #-}!Capset
                         -- | Generation of collection
                       , gen          :: {-# UNPACK #-}!Int
                         -- | Bytes copied
                       , copied       :: {-# UNPACK #-}!Word64
                         -- | Bytes of slop found
                       , slop         :: {-# UNPACK #-}!Word64
                         -- | Bytes of fragmentation, the difference between total mblock size and total block size.
                         --   When all mblocks are full of full blocks, this number is 0.
                       , frag         :: {-# UNPACK #-}!Word64
                         -- | Number of parallel garbage collection threads
                       , parNThreads  :: {-# UNPACK #-}!Int
                         -- | Maximum number of bytes copied by any single collector thread
                       , parMaxCopied :: {-# UNPACK #-}!Word64
                         -- | Total bytes copied by all collector threads
                       , parTotCopied :: {-# UNPACK #-}!Word64
                       , parBalancedCopied :: !(Maybe Word64)
                       }
  -- | Report information about currently allocation megablocks and attempts made to return them to the operating system.
  --   If your heap is fragmented then the current value will be greater than needed value but returned will be less than the difference between the two.
  | MemReturn          { heapCapset :: !Capset
                         -- | Currently allocated mblocks
                       , current :: !Word32
                         -- | The number of mblocks we would like to retain
                       , needed :: !Word32
                         -- | The number of mblocks which we returned to the OS
                       , returned :: !Word32
                       }

  -- heap statistics
  -- | A new chunk of heap has been allocated by the indicated capability set.
  | HeapAllocated      { heapCapset  :: {-# UNPACK #-}!Capset
                         -- | Allocated bytes
                       , allocBytes  :: {-# UNPACK #-}!Word64
                       }
  -- | Report the heap size, calculated by the number of megablocks currently allocated.
  | HeapSize           { heapCapset  :: {-# UNPACK #-}!Capset
                         -- | Heap size in bytes
                       , sizeBytes   :: {-# UNPACK #-}!Word64
                       }
  -- | Report the heap size, calculated by the number of blocks currently allocated.
  | BlocksSize         { heapCapset  :: {-# UNPACK #-}!Capset
                         -- | Heap size in bytes
                       , blocksSize  :: {-# UNPACK #-}!Word64
                       }
  -- | Report the live heap size.
  | HeapLive           { heapCapset  :: {-# UNPACK #-}!Capset
                         -- | Heap size in bytes
                       , liveBytes   :: {-# UNPACK #-}!Word64
                       }
  -- | Report various information about the heap configuration. Typically produced during RTS initialization.
  | HeapInfoGHC        { heapCapset    :: {-# UNPACK #-}!Capset
                         -- | Number of garbage collection generations
                       , gens          :: {-# UNPACK #-}!Int
                         -- | Maximum heap size
                       , maxHeapSize   :: {-# UNPACK #-}!Word64
                         -- | Allocation area size
                       , allocAreaSize :: {-# UNPACK #-}!Word64
                         -- | MBlock size
                       , mblockSize    :: {-# UNPACK #-}!Word64
                         -- | Block size
                       , blockSize     :: {-# UNPACK #-}!Word64
                       }

  -- adjusting the number of capabilities on the fly
  -- | A capability has been started.
  | CapCreate          { cap :: {-# UNPACK #-}!Int
                       }
  -- | A capability has been deleted.
  | CapDelete          { cap :: {-# UNPACK #-}!Int
                       }
  -- | A capability has been disabled.
  | CapDisable         { cap :: {-# UNPACK #-}!Int
                       }
  -- | A capability has been enabled.
  | CapEnable          { cap :: {-# UNPACK #-}!Int
                       }

  -- capability sets
  | CapsetCreate       { capset     :: {-# UNPACK #-}!Capset
                       , capsetType :: CapsetType
                       }
  | CapsetDelete       { capset :: {-# UNPACK #-}!Capset
                       }
  | CapsetAssignCap    { capset :: {-# UNPACK #-}!Capset
                       , cap    :: {-# UNPACK #-}!Int
                       }
  | CapsetRemoveCap    { capset :: {-# UNPACK #-}!Capset
                       , cap    :: {-# UNPACK #-}!Int
                       }

  -- program/process info
  -- | Describes the name and version of the runtime system responsible for the indicated capability set.
  | RtsIdentifier      { capset :: {-# UNPACK #-}!Capset
                       , rtsident :: !Text
                       }
  -- | Describes the command-line used to start the program.
  | ProgramArgs        { capset :: {-# UNPACK #-}!Capset
                       , args   :: [Text]
                       }
  -- | Describes the environment variables present in the program’s environment.
  | ProgramEnv         { capset :: {-# UNPACK #-}!Capset
                       , env    :: [Text]
                       }
  | OsProcessPid       { capset :: {-# UNPACK #-}!Capset
                       , pid    :: {-# UNPACK #-}!PID
                       }
  | OsProcessParentPid { capset :: {-# UNPACK #-}!Capset
                       , ppid   :: {-# UNPACK #-}!PID
                       }
  | WallClockTime      { capset :: {-# UNPACK #-}!Capset
                       , sec    :: {-# UNPACK #-}!Word64
                       , nsec   :: {-# UNPACK #-}!Word32
                       }

  -- messages
  -- | A log message from the runtime system.
  | Message            { msg :: !Text }
  -- | A user log message (from, e.g., Control.Concurrent.traceEvent).
  | UserMessage        { msg :: !Text }
  -- | A user marker (from Debug.Trace.traceMarker).
  | UserMarker         { markername :: !Text }

  -- Events emitted by a parallel RTS
   -- Program /process info (tools might prefer newer variants above)
  | Version            { version :: String }
  | ProgramInvocation  { commandline :: String }
   -- startup and shutdown (incl. real start time, not first log entry)
  | CreateMachine      { machine :: {-# UNPACK #-} !MachineId,
                         realtime    :: {-# UNPACK #-} !Timestamp}
  | KillMachine        { machine ::  {-# UNPACK #-} !MachineId }
   -- Haskell processes mgmt (thread groups that share heap and communicate)
  | CreateProcess      { process :: {-# UNPACK #-} !ProcessId }
  | KillProcess        { process :: {-# UNPACK #-} !ProcessId }
  | AssignThreadToProcess { thread :: {-# UNPACK #-} !ThreadId,
                            process :: {-# UNPACK #-} !ProcessId
                          }
   -- communication between processes
  | EdenStartReceive   { }
  | EdenEndReceive     { }
  | SendMessage        { mesTag :: !MessageTag,
                         senderProcess :: {-# UNPACK #-} !ProcessId,
                         senderThread :: {-# UNPACK #-} !ThreadId,
                         receiverMachine ::  {-# UNPACK #-} !MachineId,
                         receiverProcess :: {-# UNPACK #-} !ProcessId,
                         receiverInport :: {-# UNPACK #-} !PortId
                       }
  | ReceiveMessage     { mesTag :: !MessageTag,
                         receiverProcess :: {-# UNPACK #-} !ProcessId,
                         receiverInport :: {-# UNPACK #-} !PortId,
                         senderMachine ::  {-# UNPACK #-} !MachineId,
                         senderProcess :: {-# UNPACK #-} !ProcessId,
                         senderThread :: {-# UNPACK #-} !ThreadId,
                         messageSize :: {-# UNPACK #-} !MessageSize
                       }
  | SendReceiveLocalMessage { mesTag :: !MessageTag,
                              senderProcess :: {-# UNPACK #-} !ProcessId,
                              senderThread :: {-# UNPACK #-} !ThreadId,
                              receiverProcess :: {-# UNPACK #-} !ProcessId,
                              receiverInport :: {-# UNPACK #-} !PortId
                            }

  -- These events have been added for Mercury's benefit but are generally
  -- useful.
  | InternString       { str :: String, sId :: {-# UNPACK #-}!StringId }

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

  -- perf events
  | PerfName           { perfNum :: {-# UNPACK #-}!PerfEventTypeNum
                       , name    :: !Text
                       }
  | PerfCounter        { perfNum :: {-# UNPACK #-}!PerfEventTypeNum
                       , tid     :: {-# UNPACK #-}!KernelThreadId
                       , period  :: {-# UNPACK #-}!Word64
                       }
  | PerfTracepoint     { perfNum :: {-# UNPACK #-}!PerfEventTypeNum
                       , tid     :: {-# UNPACK #-}!KernelThreadId
                       }
  -- | A single fixed-width event emitted during program start-up describing the samples that follow.
  | HeapProfBegin      { -- | Profile ID
                         heapProfId :: !Word8
                         -- | Sampling period in nanoseconds
                       , heapProfSamplingPeriod :: !Word64
                         -- | Sample breakdown type
                       , heapProfBreakdown :: !HeapProfBreakdown
                         -- | Module filter
                       , heapProfModuleFilter :: !Text
                         -- | Closure description filter
                       , heapProfClosureDescrFilter :: !Text
                         -- | Type description filter
                       , heapProfTypeDescrFilter :: !Text
                         -- | Cost centre filter
                       , heapProfCostCentreFilter :: !Text
                         -- | Cost centre stack filter
                       , heapProfCostCentreStackFilter :: !Text
                         -- | Retainer filter
                       , heapProfRetainerFilter :: !Text
                         -- | Biography filter
                       , heapProfBiographyFilter :: !Text
                       }
  -- | A variable-length packet produced once for each cost centre,
  | HeapProfCostCentre { -- | Cost centre number
                         heapProfCostCentreId :: !Word32
                         -- | Label
                       , heapProfLabel :: !Text
                         -- | Module
                       , heapProfModule :: !Text
                         -- | Source location
                       , heapProfSrcLoc :: !Text
                         -- | bit 0: is the cost-centre a CAF?
                       , heapProfFlags :: !HeapProfFlags
                       }
  | InfoTableProv      { itInfo :: !Word64
                       , itTableName :: !Text
                       , itClosureDesc :: !Int
                       , itTyDesc :: !Text
                       , itLabel :: !Text
                       , itModule :: !Text
                       , itSrcLoc :: !Text }
  | HeapProfSampleBegin
                       { heapProfSampleEra :: !Word64
                       }
  | HeapProfSampleEnd
                       { heapProfSampleEra :: !Word64
                       }

  | HeapBioProfSampleBegin
                       { heapProfSampleEra :: !Word64
                       , heapProfSampleTime :: !Word64
                       }
  | HeapProfSampleCostCentre
                       { heapProfId :: !Word8
                       , heapProfResidency :: !Word64
                       , heapProfStackDepth :: !Word8
                       , heapProfStack :: !(VU.Vector Word32)
                       }
  | HeapProfSampleString
                       { heapProfId :: !Word8
                       , heapProfResidency :: !Word64
                       , heapProfLabel :: !Text
                       }

  | ProfSampleCostCentre
                       { profCap :: !CapNo
                       , profTicks :: !Word64
                       , profStackDepth :: !Word8
                       , profCcsStack :: !(VU.Vector Word32)
                       }
  | ProfBegin
                       { profTickInterval :: !Word64
                       }

  | UserBinaryMessage  { payload :: !B.ByteString
                       }

  | ConcMarkBegin
  | ConcMarkEnd        { concMarkedObjectCount :: !Word32
                       }
  | ConcSyncBegin
  | ConcSyncEnd
  | ConcSweepBegin
  | ConcSweepEnd
  | ConcUpdRemSetFlush { cap    :: {-# UNPACK #-}!Int
                       }
  | NonmovingHeapCensus
                       { nonmovingCensusBlkSize :: !Word16
                       , nonmovingCensusActiveSegs :: !Word32
                       , nonmovingCensusFilledSegs :: !Word32
                       , nonmovingCensusLiveBlocks :: !Word32
                       }
  | NonmovingPrunedSegments
                       { -- | The number of pruned segments.
                         nonmovingPrunedSegments :: !Word32
                       -- | The number of leftover free segments.
                       -- These could not be pruned as they belong to megablocks
                       -- with at least one non-free segment.
                       , nonmovingFreeSegments :: !Word32
                       }
  | TickyCounterDef
                       { tickyCtrDefId      :: !Word64
                       , tickyCtrDefArity   :: !Word16
                       , tickyCtrDefKinds   :: !Text
                       , tickyCtrDefName    :: !Text
                       , tickyCtrInfoTbl    :: !Word64
                       , tickyCtrJsonDesc   :: Maybe Text
                       }
  | TickyCounterSample
                       { tickyCtrSampleId         :: !Word64
                       , tickyCtrSampleEntryCount :: !Word64
                       , tickyCtrSampleAllocs     :: !Word64
                       , tickyCtrSampleAllocd     :: !Word64
                       }
  | TickyBeginSample
  deriving Show

{- [Note: Stop status in GHC-7.8.2]

In GHC-7.7, a new thread block reason "BlockedOnMVarRead" was
introduced, and placed adjacent to BlockedOnMVar (7). Therefore, event
logs produced by GHC pre-7.8.2 encode BlockedOnBlackHole and following
as 8..18, whereas GHC-7.8.2 event logs encode them as 9..19.
Later, the prior event numbering was restored for GHC-7.8.3.
See GHC bug #9003 for a discussion.

The parsers in Events.hs have to be adapted accordingly, providing
special ghc-7.8.2 parsers for the thread-stop event if GHC-7.8.2
produced the event log.
The EVENT_USER_MARKER was not present in GHC-7.6.3, and a new event
EVENT_HACK_BUG_T9003 was added in GHC-7.8.3, so we take presence of
USER_MARKER and absence of HACK_BUG_T9003 as an indication that
ghc-7.8.2 parsers should be used.
-}

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
 | BlockedOnMVarRead   -- since GHC-7.8, see [Stop status since GHC-7.7]
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

-- normal GHC encoding, see [Stop status in GHC-7.8.2]
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
 19 ->  NoStatus -- yeuch... this one does not actually exist in GHC event logs
 20 ->  BlockedOnMVarRead -- since GHC-7.8.3
 _  ->  error "mkStat"

-- GHC 7.8.2 encoding, see [Stop status in GHC-7.8.2]
mkStopStatus782 :: RawThreadStopStatus -> ThreadStopStatus
mkStopStatus782 n = case n of
 0  ->  NoStatus
 1  ->  HeapOverflow
 2  ->  StackOverflow
 3  ->  ThreadYielding
 4  ->  ThreadBlocked
 5  ->  ThreadFinished
 6  ->  ForeignCall
 7  ->  BlockedOnMVar
 8  ->  BlockedOnMVarRead -- in GHC-7.8.2
 9  ->  BlockedOnBlackHole
 10 ->  BlockedOnRead
 11 ->  BlockedOnWrite
 12 ->  BlockedOnDelay
 13 ->  BlockedOnSTM
 14 ->  BlockedOnDoProc
 15 ->  BlockedOnCCall
 16 ->  BlockedOnCCall_NoUnblockExc
 17 ->  BlockedOnMsgThrowTo
 18 ->  ThreadMigrating
 19 ->  BlockedOnMsgGlobalise
 _  ->  error "mkStat"

maxThreadStopStatusPre77, maxThreadStopStatus782, maxThreadStopStatus
    :: RawThreadStopStatus
maxThreadStopStatusPre77  = 18 -- see [Stop status in GHC-7.8.2]
maxThreadStopStatus782    = 19 -- need to distinguish three cases
maxThreadStopStatus = 20

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
{-# DEPRECATED CapEvent "CapEvents will be removed soon, now Event has a field evCap" #-}
data CapEvent
  = CapEvent { ce_cap   :: Maybe Int,
               ce_event :: Event
               -- we could UNPACK ce_event, but the Event constructor
               -- might be shared, in which case we could end up
               -- increasing the space usage.
             } deriving Show

--sync with ghc/parallel/PEOpCodes.h
data MessageTag
  = Ready | NewPE | PETIDS | Finish
  | FailPE | RFork | Connect | DataMes
  | Head | Constr | Part | Terminate
  | Packet
  -- with GUM and its variants, add:
  -- ...| Fetch | Resume | Ack
  -- ...| Fish | Schedule | Free | Reval | Shark
  deriving (Enum, Show)
offset :: RawMsgTag
offset = 0x50

-- decoder and encoder
toMsgTag :: RawMsgTag -> MessageTag
toMsgTag = toEnum . fromIntegral . (\n -> n - offset)

fromMsgTag :: MessageTag -> RawMsgTag
fromMsgTag = (+ offset) . fromIntegral . fromEnum

-- | Sample break-down types in heap profiling
data HeapProfBreakdown
  = HeapProfBreakdownCostCentre
  | HeapProfBreakdownModule
  | HeapProfBreakdownClosureDescr
  | HeapProfBreakdownTypeDescr
  | HeapProfBreakdownRetainer
  | HeapProfBreakdownBiography
  | HeapProfBreakdownClosureType
  | HeapProfBreakdownInfoTable
  | HeapProfBreakdownEra
  deriving Show

instance Binary HeapProfBreakdown where
  get = do
    n <- get :: Get Word32
    case n of
      1 -> return HeapProfBreakdownCostCentre
      2 -> return HeapProfBreakdownModule
      3 -> return HeapProfBreakdownClosureDescr
      4 -> return HeapProfBreakdownTypeDescr
      5 -> return HeapProfBreakdownRetainer
      6 -> return HeapProfBreakdownBiography
      7 -> return HeapProfBreakdownClosureType
      8 -> return HeapProfBreakdownInfoTable
      9 -> return HeapProfBreakdownEra
      _ -> fail $ "Unknown HeapProfBreakdown: " ++ show n
  put breakdown = put $ case breakdown of
    HeapProfBreakdownCostCentre -> (1 :: Word32)
    HeapProfBreakdownModule -> 2
    HeapProfBreakdownClosureDescr -> 3
    HeapProfBreakdownTypeDescr -> 4
    HeapProfBreakdownRetainer -> 5
    HeapProfBreakdownBiography -> 6
    HeapProfBreakdownClosureType -> 7
    HeapProfBreakdownInfoTable -> 8
    HeapProfBreakdownEra -> 9



newtype HeapProfFlags = HeapProfFlags Word8
  deriving (Show, Binary)

isCaf :: HeapProfFlags -> Bool
isCaf (HeapProfFlags w8) = testBit w8 0

-- Checks if the capability is not -1 (which indicates a global eventblock), so
-- has no associated capability
mkCap :: Int -> Maybe Int
mkCap cap = do
  guard $ fromIntegral cap /= (maxBound :: Word16)
  return cap
