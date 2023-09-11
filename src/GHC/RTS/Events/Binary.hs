{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module GHC.RTS.Events.Binary
  ( -- * Readers
    getHeader
  , getEvent
  , standardParsers
  , mercuryParsers
  , perfParsers
  , heapProfParsers
  , timeProfParsers
  , parRTSParsers
  , binaryEventParsers
  , tickyParsers

  -- * Writers
  , putEventLog
  , putHeader
  , putEvent

  -- * Perf events
  , nEVENT_PERF_NAME
  , nEVENT_PERF_COUNTER
  , nEVENT_PERF_TRACEPOINT

  ) where
import Control.Exception (assert)
import Control.Monad
import Data.List (intersperse)
import Data.Maybe
import Data.Int
import Prelude hiding (gcd, rem, id)

import Data.Array
import Data.Binary
import Data.Binary.Put
import qualified Data.Binary.Get as G
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Text.Encoding as TE
import qualified Data.Vector.Unboxed as VU

import GHC.RTS.EventTypes
import GHC.RTS.EventParserUtils

#define EVENTLOG_CONSTANTS_ONLY
#include "EventLogFormat.h"

getEventType :: Get EventType
getEventType = do
           etNum <- get
           size <- get :: Get EventTypeSize
           let etSize = if size == 0xffff then Nothing else Just size
           -- 0xffff indicates variable-sized event
           etDescLen <- get :: Get EventTypeDescLen
           etDesc <- getText etDescLen
           etExtraLen <- get :: Get Word32
           G.skip (fromIntegral etExtraLen)
           ete <- get :: Get Marker
           when (ete /= EVENT_ET_END) $
              fail "Event Type end marker not found."
           return (EventType etNum etDesc etSize)

getHeader :: Get Header
getHeader = do
            hdrb <- get :: Get Marker
            when (hdrb /= EVENT_HEADER_BEGIN) $
                 fail "Header begin marker not found"
            hetm <- get :: Get Marker
            when (hetm /= EVENT_HET_BEGIN) $
                 fail "Header Event Type begin marker not found"
            ets <- getEventTypes
            emark <- get :: Get Marker
            when (emark /= EVENT_HEADER_END) $
                 fail "Header end marker not found"
            db <- get :: Get Marker
            when (db /= EVENT_DATA_BEGIN) $
                  fail "My Data begin marker not found"
            return $ Header ets
     where
      getEventTypes :: Get [EventType]
      getEventTypes = do
          m <- get :: Get Marker
          case m of
             EVENT_ET_BEGIN -> do
                  et <- getEventType
                  nextET <- getEventTypes
                  return (et : nextET)
             EVENT_HET_END ->
                  return []
             _ ->
                  fail "Malformed list of Event Types in header"

getEvent :: EventParsers -> Get (Maybe Event)
getEvent (EventParsers parsers) = do
  etRef <- get :: Get EventTypeNum
  if etRef == EVENT_DATA_END
     then return Nothing
     else do !evTime   <- get
             evSpec <- parsers ! fromIntegral etRef
             return $ Just Event { evCap = undefined, .. }

--
-- standardEventParsers.
--
standardParsers :: [EventParser EventInfo]
standardParsers = [
 (FixedSizeParser EVENT_STARTUP sz_cap (do -- (n_caps)
      c <- get :: Get CapNo
      return Startup{ n_caps = fromIntegral c }
   )),

 (FixedSizeParser EVENT_BLOCK_MARKER (sz_block_size + sz_time + sz_cap) (do -- (size, end_time, cap)
      block_size <- get :: Get BlockSize
      end_time <- get :: Get Timestamp
      c <- get :: Get CapNo
      return EventBlock { end_time   = end_time,
                          cap        = fromIntegral c,
                          block_size = ((fromIntegral block_size) -
                                        (fromIntegral sz_block_event))
                        }
   )),

 (simpleEvent EVENT_REQUEST_SEQ_GC RequestSeqGC),

 (simpleEvent EVENT_REQUEST_PAR_GC RequestParGC),

 (simpleEvent EVENT_GC_START StartGC),

 (simpleEvent EVENT_GC_WORK GCWork),

 (simpleEvent EVENT_GC_IDLE GCIdle),

 (simpleEvent EVENT_GC_DONE GCDone),

 (simpleEvent EVENT_GC_END EndGC),

 (simpleEvent EVENT_GC_GLOBAL_SYNC GlobalSyncGC),

 (FixedSizeParser EVENT_GC_STATS_GHC (sz_capset + 2 + 5*8 + 4) (do  -- (heap_capset, generation, copied_bytes, slop_bytes, frag_bytes, par_n_threads, par_max_copied, par_tot_copied)
      heapCapset   <- get
      gen          <- get :: Get Word16
      copied       <- get :: Get Word64
      slop         <- get :: Get Word64
      frag         <- get :: Get Word64
      parNThreads  <- get :: Get Word32
      parMaxCopied <- get :: Get Word64
      parTotCopied <- get :: Get Word64
      return GCStatsGHC{ gen = fromIntegral gen
                       , parNThreads = fromIntegral parNThreads
                       , parBalancedCopied = Nothing
                       , ..}
 )),

 (FixedSizeParser EVENT_GC_STATS_GHC (sz_capset + 2 + 5*8 + 4 + 8) (do  -- (heap_capset, generation, copied_bytes, slop_bytes, frag_bytes, par_n_threads, par_max_copied, par_tot_copied, par_balanced_copied)
      heapCapset   <- get
      gen          <- get :: Get Word16
      copied       <- get :: Get Word64
      slop         <- get :: Get Word64
      frag         <- get :: Get Word64
      parNThreads  <- get :: Get Word32
      parMaxCopied <- get :: Get Word64
      parTotCopied <- get :: Get Word64
      parBalancedCopied <- get :: Get Word64
      return GCStatsGHC{ gen = fromIntegral gen
                       , parNThreads = fromIntegral parNThreads
                       , parBalancedCopied = Just parBalancedCopied
                       , ..}
 )),

 (FixedSizeParser EVENT_MEM_RETURN (sz_capset + 3*4) (do
      heapCapset   <- get
      current      <- get :: Get Word32
      needed       <- get :: Get Word32
      returned     <- get :: Get Word32
      return $! MemReturn{ current = current
                       , needed = needed
                       , returned = returned
                       , ..}
 )),

 (FixedSizeParser EVENT_HEAP_ALLOCATED (sz_capset + 8) (do  -- (heap_capset, alloc_bytes)
      heapCapset <- get
      allocBytes <- get
      return HeapAllocated{..}
 )),

 (FixedSizeParser EVENT_HEAP_SIZE (sz_capset + 8) (do  -- (heap_capset, size_bytes)
      heapCapset <- get
      sizeBytes  <- get
      return HeapSize{..}
 )),
 (FixedSizeParser EVENT_BLOCKS_SIZE (sz_capset + 8) (do  -- (heap_capset, blocks_size)
      heapCapset <- get
      blocksSize  <- get
      return $! BlocksSize{..}
 )),

 (FixedSizeParser EVENT_HEAP_LIVE (sz_capset + 8) (do  -- (heap_capset, live_bytes)
      heapCapset <- get
      liveBytes  <- get
      return HeapLive{..}
 )),

 (FixedSizeParser EVENT_HEAP_INFO_GHC (sz_capset + 2 + 4*8) (do  -- (heap_capset, n_generations, max_heap_size, alloc_area_size, mblock_size, block_size)
      heapCapset    <- get
      gens          <- get :: Get Word16
      maxHeapSize   <- get :: Get Word64
      allocAreaSize <- get :: Get Word64
      mblockSize    <- get :: Get Word64
      blockSize     <- get :: Get Word64
      return HeapInfoGHC{gens = fromIntegral gens, ..}
 )),

 (FixedSizeParser EVENT_CAP_CREATE (sz_cap) (do  -- (cap)
      cap <- get :: Get CapNo
      return CapCreate{cap = fromIntegral cap}
 )),

 (FixedSizeParser EVENT_CAP_DELETE (sz_cap) (do  -- (cap)
      cap <- get :: Get CapNo
      return CapDelete{cap = fromIntegral cap}
 )),

 (FixedSizeParser EVENT_CAP_DISABLE (sz_cap) (do  -- (cap)
      cap <- get :: Get CapNo
      return CapDisable{cap = fromIntegral cap}
 )),

 (FixedSizeParser EVENT_CAP_ENABLE (sz_cap) (do  -- (cap)
      cap <- get :: Get CapNo
      return CapEnable{cap = fromIntegral cap}
 )),

 (FixedSizeParser EVENT_CAPSET_CREATE (sz_capset + sz_capset_type) (do -- (capset, capset_type)
      cs <- get
      ct <- fmap mkCapsetType get
      return CapsetCreate{capset=cs,capsetType=ct}
   )),

 (FixedSizeParser EVENT_CAPSET_DELETE sz_capset (do -- (capset)
      cs <- get
      return CapsetDelete{capset=cs}
   )),

 (FixedSizeParser EVENT_CAPSET_ASSIGN_CAP (sz_capset + sz_cap) (do -- (capset, cap)
      cs <- get
      cp <- get :: Get CapNo
      return CapsetAssignCap{capset=cs,cap=fromIntegral cp}
   )),

 (FixedSizeParser EVENT_CAPSET_REMOVE_CAP (sz_capset + sz_cap) (do -- (capset, cap)
      cs <- get
      cp <- get :: Get CapNo
      return CapsetRemoveCap{capset=cs,cap=fromIntegral cp}
   )),

 (FixedSizeParser EVENT_OSPROCESS_PID (sz_capset + sz_pid) (do -- (capset, pid)
      cs <- get
      pd <- get
      return OsProcessPid{capset=cs,pid=pd}
   )),

 (FixedSizeParser EVENT_OSPROCESS_PPID (sz_capset + sz_pid) (do -- (capset, ppid)
      cs <- get
      pd <- get
      return OsProcessParentPid{capset=cs,ppid=pd}
  )),

 (FixedSizeParser EVENT_WALL_CLOCK_TIME (sz_capset + 8 + 4) (do -- (capset, unix_epoch_seconds, nanoseconds)
      cs <- get
      s  <- get
      ns <- get
      return WallClockTime{capset=cs,sec=s,nsec=ns}
  )),

 (VariableSizeParser EVENT_LOG_MSG (do -- (msg)
      num <- get :: Get Word16
      string <- getText num
      return Message{ msg = string }
   )),
 (VariableSizeParser EVENT_USER_MSG (do -- (msg)
      num <- get :: Get Word16
      string <- getText num
      return UserMessage{ msg = string }
   )),
    (VariableSizeParser EVENT_USER_MARKER (do -- (markername)
      num <- get :: Get Word16
      string <- getText num
      return UserMarker{ markername = string }
   )),
 (VariableSizeParser EVENT_PROGRAM_ARGS (do -- (capset, [arg])
      num <- get :: Get Word16
      cs <- get
      string <- getText (num - sz_capset)
      return ProgramArgs
        { capset = cs
        , args = T.splitOn "\0" $ T.dropWhileEnd (== '\0') string }
   )),
 (VariableSizeParser EVENT_PROGRAM_ENV (do -- (capset, [arg])
      num <- get :: Get Word16
      cs <- get
      string <- getText (num - sz_capset)
      return ProgramEnv
        { capset = cs
        , env = T.splitOn "\0" $ T.dropWhileEnd (== '\0') string }
   )),
 (VariableSizeParser EVENT_RTS_IDENTIFIER (do -- (capset, str)
      num <- get :: Get Word16
      cs <- get
      string <- getText (num - sz_capset)
      return RtsIdentifier{ capset = cs
                          , rtsident = string }
   )),

 (VariableSizeParser EVENT_INTERN_STRING (do -- (str, id)
      num <- get :: Get Word16
      string <- getString (num - sz_string_id)
      sId <- get :: Get StringId
      return (InternString string sId)
    )),

 (VariableSizeParser EVENT_THREAD_LABEL (do -- (thread, str)
      num <- get :: Get Word16
      tid <- get
      str <- getText (num - sz_tid)
      return ThreadLabel{ thread      = tid
                        , threadlabel = str }
    )),

 (simpleEvent EVENT_CONC_MARK_BEGIN ConcMarkBegin),
 (FixedSizeParser EVENT_CONC_MARK_END 4 (do -- (marked_object_count)
      num <- get :: Get Word32
      return ConcMarkEnd{ concMarkedObjectCount = num }
    )),
 (simpleEvent EVENT_CONC_SYNC_BEGIN ConcSyncBegin),
 (simpleEvent EVENT_CONC_SYNC_END ConcSyncEnd),
 (simpleEvent EVENT_CONC_SWEEP_BEGIN ConcSweepBegin),
 (simpleEvent EVENT_CONC_SWEEP_END ConcSweepEnd),
 (FixedSizeParser EVENT_CONC_UPD_REM_SET_FLUSH sz_cap (do -- (cap)
      cap <- get :: Get CapNo
      return ConcUpdRemSetFlush{ cap = fromIntegral cap }
    )),
 -- Before GHC issue 23340 event where we are given the logarithm of the block size
 (FixedSizeParser EVENT_NONMOVING_HEAP_CENSUS 13 (do -- (blk_size, active_segs, filled_segs, live_blks)
      nonmovingCensusLogBlkSize <- get :: Get Word8
      let nonmovingCensusBlkSize = 2^nonmovingCensusLogBlkSize
      nonmovingCensusActiveSegs <- get :: Get Word32
      nonmovingCensusFilledSegs <- get :: Get Word32
      nonmovingCensusLiveBlocks <- get :: Get Word32
      return NonmovingHeapCensus{..}
    )),
 (FixedSizeParser EVENT_NONMOVING_HEAP_CENSUS 14 (do -- (blk_size, active_segs, filled_segs, live_blks)
      nonmovingCensusBlkSize <- get :: Get Word16
      nonmovingCensusActiveSegs <- get :: Get Word32
      nonmovingCensusFilledSegs <- get :: Get Word32
      nonmovingCensusLiveBlocks <- get :: Get Word32
      return NonmovingHeapCensus{..}
    )),

 (FixedSizeParser EVENT_CREATE_THREAD sz_tid (do  -- (thread)
      t <- get
      return CreateThread{thread=t}
   )),

 (FixedSizeParser EVENT_RUN_THREAD sz_tid (do  --  (thread)
      t <- get
      return RunThread{thread=t}
   )),

 (FixedSizeParser EVENT_THREAD_RUNNABLE sz_tid (do  -- (thread)
      t <- get
      return ThreadRunnable{thread=t}
   )),

 (FixedSizeParser EVENT_MIGRATE_THREAD (sz_tid + sz_cap) (do  --  (thread, newCap)
      t  <- get
      nc <- get :: Get CapNo
      return MigrateThread{thread=t,newCap=fromIntegral nc}
   )),

 (FixedSizeParser EVENT_CREATE_SPARK_THREAD sz_tid (do  -- (sparkThread)
      st <- get :: Get ThreadId
      return CreateSparkThread{sparkThread=st}
   )),

 (FixedSizeParser EVENT_SPARK_COUNTERS (7*8) (do -- (crt,dud,ovf,cnv,gcd,fiz,rem)
      crt <- get :: Get Word64
      dud <- get :: Get Word64
      ovf <- get :: Get Word64
      cnv <- get :: Get Word64
      gcd <- get :: Get Word64
      fiz <- get :: Get Word64
      rem <- get :: Get Word64
      return SparkCounters{sparksCreated    = crt, sparksDud       = dud,
                           sparksOverflowed = ovf, sparksConverted = cnv,
                           -- Warning: order of fiz and gcd reversed!
                           sparksFizzled    = fiz, sparksGCd       = gcd,
                           sparksRemaining  = rem}
   )),

 (simpleEvent EVENT_SPARK_CREATE   SparkCreate),
 (simpleEvent EVENT_SPARK_DUD      SparkDud),
 (simpleEvent EVENT_SPARK_OVERFLOW SparkOverflow),
 (simpleEvent EVENT_SPARK_RUN      SparkRun),
 (FixedSizeParser EVENT_SPARK_STEAL sz_cap (do  -- (victimCap)
      vc <- get :: Get CapNo
      return SparkSteal{victimCap=fromIntegral vc}
   )),
 (simpleEvent EVENT_SPARK_FIZZLE   SparkFizzle),
 (simpleEvent EVENT_SPARK_GC       SparkGC),

 (FixedSizeParser EVENT_TASK_CREATE (sz_taskid + sz_cap + sz_kernel_tid) (do  -- (taskID, cap, tid)
      taskId <- get :: Get TaskId
      cap    <- get :: Get CapNo
      tid    <- get :: Get KernelThreadId
      return TaskCreate{ taskId, cap = fromIntegral cap, tid }
   )),
 (FixedSizeParser EVENT_TASK_MIGRATE (sz_taskid + sz_cap*2) (do  -- (taskID, cap, new_cap)
      taskId  <- get :: Get TaskId
      cap     <- get :: Get CapNo
      new_cap <- get :: Get CapNo
      return TaskMigrate{ taskId, cap = fromIntegral cap
                                , new_cap = fromIntegral new_cap
                        }
   )),
 (FixedSizeParser EVENT_TASK_DELETE (sz_taskid) (do  -- (taskID)
      taskId <- get :: Get TaskId
      return TaskDelete{ taskId }
   )),

 (FixedSizeParser EVENT_THREAD_WAKEUP (sz_tid + sz_cap) (do  -- (thread, other_cap)
      t <- get
      oc <- get :: Get CapNo
      return WakeupThread{thread=t,otherCap=fromIntegral oc}
   )),

 (FixedSizeParser EVENT_STOP_THREAD (sz_tid + sz_th_stop_status + sz_tid)
    (do
      -- (thread, status, info)
      t <- get
      s <- get :: Get RawThreadStopStatus
      i <- get :: Get ThreadId
      return StopThread{thread = t,
                        status = case () of
                                  _ | s > maxThreadStopStatus
                                    -> NoStatus
                                    | s == 8 {- XXX yeuch -}
                                      -- post-7.8.2: 8==BlockedOnBlackhole
                                    -> BlockedOnBlackHoleOwnedBy i
                                    | otherwise
                                    -> mkStopStatus s}
    ))
 ]


-- Parsers for parallel events. Parameter is the thread_id size, to create
-- ghc6-parsers (using the wrong size) where necessary.
parRTSParsers :: EventTypeSize -> [EventParser EventInfo]
parRTSParsers sz_tid' = [
 (VariableSizeParser EVENT_VERSION (do -- (version)
      num <- get :: Get Word16
      string <- getString num
      return Version{ version = string }
   )),

 (VariableSizeParser EVENT_PROGRAM_INVOCATION (do -- (cmd. line)
      num <- get :: Get Word16
      string <- getString num
      return ProgramInvocation{ commandline = string }
   )),

 (simpleEvent EVENT_EDEN_START_RECEIVE EdenStartReceive),
 (simpleEvent EVENT_EDEN_END_RECEIVE   EdenEndReceive),

 (FixedSizeParser EVENT_CREATE_PROCESS sz_procid
    (do p <- get
        return CreateProcess{ process = p })
 ),

 (FixedSizeParser EVENT_KILL_PROCESS sz_procid
    (do p <- get
        return KillProcess{ process = p })
 ),

 (FixedSizeParser EVENT_ASSIGN_THREAD_TO_PROCESS (sz_tid' + sz_procid)
    (do t <- get
        p <- get
        return AssignThreadToProcess { thread = t, process = p })
 ),

 (FixedSizeParser EVENT_CREATE_MACHINE (sz_mid + sz_realtime)
    (do m <- get
        t <- get
        return CreateMachine { machine = m, realtime = t })
 ),

 (FixedSizeParser EVENT_KILL_MACHINE sz_mid
    (do m <- get :: Get MachineId
        return KillMachine { machine = m })
 ),

 (FixedSizeParser EVENT_SEND_MESSAGE
    (sz_msgtag + 2*sz_procid + 2*sz_tid' + sz_mid)
    (do tag <- get :: Get RawMsgTag
        sP  <- get :: Get ProcessId
        sT  <- get :: Get ThreadId
        rM  <- get :: Get MachineId
        rP  <- get :: Get ProcessId
        rIP <- get :: Get PortId
        return SendMessage { mesTag = toMsgTag tag,
                             senderProcess = sP,
                             senderThread = sT,
                             receiverMachine = rM,
                             receiverProcess = rP,
                             receiverInport = rIP
                           })
 ),

 (FixedSizeParser EVENT_RECEIVE_MESSAGE
    (sz_msgtag + 2*sz_procid + 2*sz_tid' + sz_mid + sz_mes)
    (do tag <- get :: Get Word8
        rP  <- get :: Get ProcessId
        rIP <- get :: Get PortId
        sM  <- get :: Get MachineId
        sP  <- get :: Get ProcessId
        sT  <- get :: Get ThreadId
        mS  <- get :: Get MessageSize
        return  ReceiveMessage { mesTag = toMsgTag tag,
                                 receiverProcess = rP,
                                 receiverInport = rIP,
                                 senderMachine = sM,
                                 senderProcess = sP,
                                 senderThread= sT,
                                 messageSize = mS
                               })
 ),

 (FixedSizeParser EVENT_SEND_RECEIVE_LOCAL_MESSAGE
    (sz_msgtag + 2*sz_procid + 2*sz_tid')
    (do tag <- get :: Get Word8
        sP  <- get :: Get ProcessId
        sT  <- get :: Get ThreadId
        rP  <- get :: Get ProcessId
        rIP <- get :: Get PortId
        return SendReceiveLocalMessage { mesTag = toMsgTag tag,
                                         senderProcess = sP,
                                         senderThread = sT,
                                         receiverProcess = rP,
                                         receiverInport = rIP
                                       })
 )]

mercuryParsers :: [EventParser EventInfo]
mercuryParsers = [
 (FixedSizeParser EVENT_MER_START_PAR_CONJUNCTION
    (sz_par_conj_dyn_id + sz_par_conj_static_id)
    (do dyn_id <- get
        static_id <- get
        return (MerStartParConjunction dyn_id static_id))
 ),

 (FixedSizeParser EVENT_MER_STOP_PAR_CONJUNCTION sz_par_conj_dyn_id
    (do dyn_id <- get
        return (MerEndParConjunction dyn_id))
 ),

 (FixedSizeParser EVENT_MER_STOP_PAR_CONJUNCT sz_par_conj_dyn_id
    (do dyn_id <- get
        return (MerEndParConjunct dyn_id))
 ),

 (FixedSizeParser EVENT_MER_CREATE_SPARK (sz_par_conj_dyn_id + sz_spark_id)
    (do dyn_id <- get
        spark_id <- get
        return (MerCreateSpark dyn_id spark_id))
 ),

 (FixedSizeParser EVENT_MER_FUT_CREATE (sz_future_id + sz_string_id)
    (do future_id <- get
        name_id <- get
        return (MerFutureCreate future_id name_id))
 ),

 (FixedSizeParser EVENT_MER_FUT_WAIT_NOSUSPEND (sz_future_id)
    (do future_id <- get
        return (MerFutureWaitNosuspend future_id))
 ),

 (FixedSizeParser EVENT_MER_FUT_WAIT_SUSPENDED (sz_future_id)
    (do future_id <- get
        return (MerFutureWaitSuspended future_id))
 ),

 (FixedSizeParser EVENT_MER_FUT_SIGNAL (sz_future_id)
    (do future_id <- get
        return (MerFutureSignal future_id))
 ),

 (simpleEvent EVENT_MER_LOOKING_FOR_GLOBAL_CONTEXT MerLookingForGlobalThread),
 (simpleEvent EVENT_MER_WORK_STEALING MerWorkStealing),
 (simpleEvent EVENT_MER_LOOKING_FOR_LOCAL_SPARK MerLookingForLocalSpark),

 (FixedSizeParser EVENT_MER_RELEASE_CONTEXT sz_tid
    (do thread_id <- get
        return (MerReleaseThread thread_id))
 ),

 (simpleEvent EVENT_MER_ENGINE_SLEEPING MerCapSleeping),
 (simpleEvent EVENT_MER_CALLING_MAIN MerCallingMain)

 ]

perfParsers :: [EventParser EventInfo]
perfParsers = [
 (VariableSizeParser EVENT_PERF_NAME (do -- (perf_num, name)
      num     <- get :: Get Word16
      perfNum <- get
      name    <- getText (num - sz_perf_num)
      return PerfName{perfNum, name}
   )),

 (FixedSizeParser EVENT_PERF_COUNTER (sz_perf_num + sz_kernel_tid + 8) (do -- (perf_num, tid, period)
      perfNum <- get
      tid     <- get
      period  <- get
      return PerfCounter{perfNum, tid, period}
  )),

 (FixedSizeParser EVENT_PERF_TRACEPOINT (sz_perf_num + sz_kernel_tid) (do -- (perf_num, tid)
      perfNum <- get
      tid     <- get
      return PerfTracepoint{perfNum, tid}
  ))
 ]

heapProfParsers :: [EventParser EventInfo]
heapProfParsers =
  [ VariableSizeParser EVENT_HEAP_PROF_BEGIN $ do
    payloadLen <- get :: Get Word16
    heapProfId <- get
    heapProfSamplingPeriod <- get
    heapProfBreakdown <- get
    heapProfModuleFilter <- getTextNul
    heapProfClosureDescrFilter <- getTextNul
    heapProfTypeDescrFilter <- getTextNul
    heapProfCostCentreFilter <- getTextNul
    heapProfCostCentreStackFilter <- getTextNul
    heapProfRetainerFilter <- getTextNul
    heapProfBiographyFilter <- getTextNul
    assert
      (fromIntegral payloadLen == sum
        [ 1 -- heapProfId
        , 8 -- heapProfSamplingPeriod
        , 4 -- heapProfBreakdown
        , textByteLen heapProfModuleFilter
        , textByteLen heapProfClosureDescrFilter
        , textByteLen heapProfTypeDescrFilter
        , textByteLen heapProfCostCentreFilter
        , textByteLen heapProfCostCentreStackFilter
        , textByteLen heapProfRetainerFilter
        , textByteLen heapProfBiographyFilter
        ])
      (return ())
    return $! HeapProfBegin {..}
  , VariableSizeParser EVENT_HEAP_PROF_COST_CENTRE $ do
    payloadLen <- get :: Get Word16
    heapProfCostCentreId <- get
    heapProfLabel <- getTextNul
    heapProfModule <- getTextNul
    heapProfSrcLoc <- getTextNul
    heapProfFlags <- get
    assert
      (fromIntegral payloadLen == sum
        [ 4 -- heapProfCostCentreId
        , textByteLen heapProfLabel
        , textByteLen heapProfModule
        , textByteLen heapProfSrcLoc
        , 1 -- heapProfFlags
        ])
      (return ())
    return $! HeapProfCostCentre {..}
  , VariableSizeParser EVENT_IPE $ do
    payloadLen <- get :: Get Word16
    itInfo <- get
    itTableName <- getTextNul
    itClosureDescText <- getTextNul
    itClosureDesc <- either fail (return . fst) (TR.decimal itClosureDescText)
    itTyDesc <- getTextNul
    itLabel <- getTextNul
    itModule <- getTextNul
    itSrcLoc <- getTextNul
    assert
      (fromIntegral payloadLen == sum
        [ 8 -- itInfo
        , textByteLen itTableName
        , textByteLen itClosureDescText
        , textByteLen itTyDesc
        , textByteLen itLabel
        , textByteLen itModule
        , textByteLen itSrcLoc
        ])
      (return ())
    return $! InfoTableProv {..}
  , FixedSizeParser EVENT_HEAP_PROF_SAMPLE_BEGIN 8 $ do
    heapProfSampleEra <- get
    return $! HeapProfSampleBegin {..}
  , FixedSizeParser EVENT_HEAP_PROF_SAMPLE_END 8 $ do
    heapProfSampleEra <- get
    return $! HeapProfSampleEnd {..}
  , FixedSizeParser EVENT_HEAP_BIO_PROF_SAMPLE_BEGIN 16 $ do
    heapProfSampleEra <- get
    heapProfSampleTime <- get
    return $! HeapBioProfSampleBegin {..}
  , VariableSizeParser EVENT_HEAP_PROF_SAMPLE_COST_CENTRE $ do
    payloadLen <- get :: Get Word16
    heapProfId <- get
    heapProfResidency <- get
    heapProfStackDepth <- get
    heapProfStack <- VU.replicateM (fromIntegral heapProfStackDepth) get
    assert
      ((fromIntegral payloadLen :: Int) == sum
        [ 1 -- heapProfId
        , 8 -- heapProfResidency
        , 1 -- heapProfStackDepth
        , fromIntegral heapProfStackDepth * 4
        ])
      (return ())
    return $! HeapProfSampleCostCentre {..}
  , VariableSizeParser EVENT_HEAP_PROF_SAMPLE_STRING $ do
    payloadLen <- get :: Get Word16
    heapProfId <- get
    heapProfResidency <- get
    heapProfLabel <- getTextNul
    assert
      (fromIntegral payloadLen == sum
        [ 1 -- heapProfId
        , 8 -- heapProfResidency
        , textByteLen heapProfLabel
        ])
      (return ())
    return $! HeapProfSampleString {..}
  ]

timeProfParsers :: [EventParser EventInfo]
timeProfParsers = [
  FixedSizeParser EVENT_PROF_BEGIN 8 $ do
    profTickInterval <- get
    return $! ProfBegin{..}
  , VariableSizeParser EVENT_PROF_SAMPLE_COST_CENTRE $ do
    payloadLen <- get :: Get Word16
    profCapset <- get
    profTicks <- get
    profStackDepth <- get
    profCcsStack <- VU.replicateM (fromIntegral profStackDepth) get
    assert
      ((fromIntegral payloadLen :: Int) == sum
        [ 4
        , 8 -- ticks
        , 1 -- stack depth
        , fromIntegral profStackDepth * 4
        ])
      (return ())
    return $! ProfSampleCostCentre {..} ]

binaryEventParsers :: [EventParser EventInfo]
binaryEventParsers =
  [ VariableSizeParser EVENT_USER_BINARY_MSG $ do
    payloadLen <- get :: Get Word16
    payload <- G.getByteString $ fromIntegral payloadLen
    return $! UserBinaryMessage { payload }
  ]

-- | Reads the `a` object, and if that didn't consume the complete
-- event skip over the leftover data.
skipExtra :: Word16 -> Get a -> Get a
skipExtra expected_size get_body = do
  bytes_read <- G.bytesRead
  res <- get_body
  bytes_read_end <- G.bytesRead
  let total_size = bytes_read_end - bytes_read
      to_skip  = fromIntegral expected_size - total_size
  when (to_skip < 0) (fail "Negative to_skip")
  skip to_skip
  return res

-- | Skip extra stuff at the end of a variable sized event
-- For forwards compatability (allowing newer events to be read with older versions)
variableSizeParser :: Int -- ^ The identifier for the event
                   -> (Word16 -> Int64 -> Get a) -- ^ A continuation to parse the body of the event
                   -> EventParser a
variableSizeParser event_type body_parser = do
  VariableSizeParser event_type $ do
    payloadLen         <- get :: Get Word16
    bytes_read <- G.bytesRead
    skipExtra payloadLen (body_parser payloadLen (fromIntegral bytes_read))

-- | If we have already got everything then return the default value, otherwise get
-- For backwards compatability (to allow older events to be read with newer versions)
optionalGet :: (Binary b)
            => Word16 -- ^ Expected size
            -> Int64  -- ^ Starting byte offset
            -> b      -- ^ Default value
            -> Get b
            -> Get b
optionalGet expected_size bytes_read def get_this = do
  bytes_read_end <- G.bytesRead
  let total_size = bytes_read_end - bytes_read
  if fromIntegral expected_size == total_size then return def else get_this

tickyParsers :: [EventParser EventInfo]
tickyParsers =
  [ variableSizeParser EVENT_TICKY_COUNTER_DEF $ \payloadLen start_bytes -> do
    tickyCtrDefId      <- get
    tickyCtrDefArity   <- get
    tickyCtrDefKinds   <- getTextNul
    tickyCtrDefName    <- getTextNul
    tickyCtrInfoTbl    <- optionalGet payloadLen start_bytes (0 :: Word64) get
    tickyCtrJsonDesc   <- optionalGet payloadLen start_bytes Nothing (Just <$> getTextNul)
    assert (fromIntegral payloadLen ==
      (sum
        [ 8 -- tickyCtrDefId
        , 2 -- tickyCtrDefArity
        , textByteLen tickyCtrDefKinds
        , textByteLen tickyCtrDefName
        , 8 -- tickyCtrInfoTbl
        , maybe 0 textByteLen tickyCtrJsonDesc
        ]))
        (return ())
    return $! TickyCounterDef{..}
  , FixedSizeParser EVENT_TICKY_COUNTER_SAMPLE (8*4) $ do
    tickyCtrSampleId         <- get
    tickyCtrSampleEntryCount <- get
    tickyCtrSampleAllocs     <- get
    tickyCtrSampleAllocd     <- get
    return $! TickyCounterSample{..}
  , simpleEvent EVENT_TICKY_BEGIN_SAMPLE TickyBeginSample
  ]

-- | String byte length in the event log format. It includes
-- 1 byte for NUL.
textByteLen :: T.Text -> Int
textByteLen = (+1) . B.length . TE.encodeUtf8

-----------------------------------------------------------

putE :: Binary a => a -> PutM ()
putE = put

putType :: EventTypeNum -> PutM ()
putType = putE

putCap :: Int -> PutM ()
putCap c = putE (fromIntegral c :: CapNo)

putMarker :: Word32 -> PutM ()
putMarker = putE

putEventLog :: EventLog -> PutM ()
putEventLog (EventLog hdr es) = do
    putHeader hdr
    putData es

putHeader :: Header -> PutM ()
putHeader (Header ets) = do
    putMarker EVENT_HEADER_BEGIN
    putMarker EVENT_HET_BEGIN
    mapM_ putEventType ets
    putMarker EVENT_HET_END
    putMarker EVENT_HEADER_END
 where
    putEventType (EventType n (TE.encodeUtf8 -> d) msz) = do
        putMarker EVENT_ET_BEGIN
        putType n
        putE $ fromMaybe 0xffff msz
        putE (fromIntegral $ B.length d :: EventTypeDescLen)
        putByteString d
        -- the event type header allows for extra data, which we don't use:
        putE (0 :: Word32)
        putMarker EVENT_ET_END

putData :: Data -> PutM ()
putData (Data es) = do
    putMarker EVENT_DATA_BEGIN -- Word32
    mapM_ putEvent es
    putType EVENT_DATA_END -- Word16

eventTypeNum :: EventInfo -> EventTypeNum
eventTypeNum e = case e of
    CreateThread {} -> EVENT_CREATE_THREAD
    RunThread {} -> EVENT_RUN_THREAD
    StopThread {} -> EVENT_STOP_THREAD
    ThreadRunnable {} -> EVENT_THREAD_RUNNABLE
    MigrateThread {} -> EVENT_MIGRATE_THREAD
    Shutdown {} -> EVENT_SHUTDOWN
    WakeupThread {} -> EVENT_THREAD_WAKEUP
    ThreadLabel {}  -> EVENT_THREAD_LABEL
    StartGC {} -> EVENT_GC_START
    EndGC {} -> EVENT_GC_END
    GlobalSyncGC {} -> EVENT_GC_GLOBAL_SYNC
    RequestSeqGC {} -> EVENT_REQUEST_SEQ_GC
    RequestParGC {} -> EVENT_REQUEST_PAR_GC
    CreateSparkThread {} -> EVENT_CREATE_SPARK_THREAD
    SparkCounters {} -> EVENT_SPARK_COUNTERS
    SparkCreate   {} -> EVENT_SPARK_CREATE
    SparkDud      {} -> EVENT_SPARK_DUD
    SparkOverflow {} -> EVENT_SPARK_OVERFLOW
    SparkRun      {} -> EVENT_SPARK_RUN
    SparkSteal    {} -> EVENT_SPARK_STEAL
    SparkFizzle   {} -> EVENT_SPARK_FIZZLE
    SparkGC       {} -> EVENT_SPARK_GC
    TaskCreate  {} -> EVENT_TASK_CREATE
    TaskMigrate {} -> EVENT_TASK_MIGRATE
    TaskDelete  {} -> EVENT_TASK_DELETE
    Message {} -> EVENT_LOG_MSG
    Startup {} -> EVENT_STARTUP
    EventBlock {} -> EVENT_BLOCK_MARKER
    UserMessage {} -> EVENT_USER_MSG
    UserMarker  {} -> EVENT_USER_MARKER
    GCIdle {} -> EVENT_GC_IDLE
    GCWork {} -> EVENT_GC_WORK
    GCDone {} -> EVENT_GC_DONE
    GCStatsGHC{} -> EVENT_GC_STATS_GHC
    HeapAllocated{} -> EVENT_HEAP_ALLOCATED
    HeapSize{} -> EVENT_HEAP_SIZE
    BlocksSize{} -> EVENT_BLOCKS_SIZE
    HeapLive{} -> EVENT_HEAP_LIVE
    HeapInfoGHC{} -> EVENT_HEAP_INFO_GHC
    CapCreate{} -> EVENT_CAP_CREATE
    CapDelete{} -> EVENT_CAP_DELETE
    CapDisable{} -> EVENT_CAP_DISABLE
    CapEnable{} -> EVENT_CAP_ENABLE
    CapsetCreate {} -> EVENT_CAPSET_CREATE
    CapsetDelete {} -> EVENT_CAPSET_DELETE
    CapsetAssignCap {} -> EVENT_CAPSET_ASSIGN_CAP
    CapsetRemoveCap {} -> EVENT_CAPSET_REMOVE_CAP
    RtsIdentifier {} -> EVENT_RTS_IDENTIFIER
    ProgramArgs {} -> EVENT_PROGRAM_ARGS
    ProgramEnv {} -> EVENT_PROGRAM_ENV
    OsProcessPid {} -> EVENT_OSPROCESS_PID
    OsProcessParentPid{} -> EVENT_OSPROCESS_PPID
    WallClockTime{} -> EVENT_WALL_CLOCK_TIME
    UnknownEvent {} -> error "eventTypeNum UnknownEvent"
    InternString {} -> EVENT_INTERN_STRING
    Version {} -> EVENT_VERSION
    ProgramInvocation {} -> EVENT_PROGRAM_INVOCATION
    EdenStartReceive {} -> EVENT_EDEN_START_RECEIVE
    EdenEndReceive {} -> EVENT_EDEN_END_RECEIVE
    CreateProcess {} -> EVENT_CREATE_PROCESS
    KillProcess {} -> EVENT_KILL_PROCESS
    AssignThreadToProcess {} -> EVENT_ASSIGN_THREAD_TO_PROCESS
    CreateMachine {} -> EVENT_CREATE_MACHINE
    KillMachine {} -> EVENT_KILL_MACHINE
    SendMessage {} -> EVENT_SEND_MESSAGE
    ReceiveMessage {} -> EVENT_RECEIVE_MESSAGE
    SendReceiveLocalMessage {} -> EVENT_SEND_RECEIVE_LOCAL_MESSAGE
    MerStartParConjunction {} -> EVENT_MER_START_PAR_CONJUNCTION
    MerEndParConjunction _ -> EVENT_MER_STOP_PAR_CONJUNCTION
    MerEndParConjunct _ -> EVENT_MER_STOP_PAR_CONJUNCT
    MerCreateSpark {} -> EVENT_MER_CREATE_SPARK
    MerFutureCreate {} -> EVENT_MER_FUT_CREATE
    MerFutureWaitNosuspend _ -> EVENT_MER_FUT_WAIT_NOSUSPEND
    MerFutureWaitSuspended _ -> EVENT_MER_FUT_WAIT_SUSPENDED
    MerFutureSignal _ -> EVENT_MER_FUT_SIGNAL
    MerLookingForGlobalThread -> EVENT_MER_LOOKING_FOR_GLOBAL_CONTEXT
    MerWorkStealing -> EVENT_MER_WORK_STEALING
    MerLookingForLocalSpark -> EVENT_MER_LOOKING_FOR_LOCAL_SPARK
    MerReleaseThread _ -> EVENT_MER_RELEASE_CONTEXT
    MerCapSleeping -> EVENT_MER_ENGINE_SLEEPING
    MerCallingMain -> EVENT_MER_CALLING_MAIN
    PerfName       {} -> nEVENT_PERF_NAME
    PerfCounter    {} -> nEVENT_PERF_COUNTER
    PerfTracepoint {} -> nEVENT_PERF_TRACEPOINT
    HeapProfBegin {} -> EVENT_HEAP_PROF_BEGIN
    HeapProfCostCentre {} -> EVENT_HEAP_PROF_COST_CENTRE
    HeapProfSampleBegin {} -> EVENT_HEAP_PROF_SAMPLE_BEGIN
    HeapProfSampleEnd {} -> EVENT_HEAP_PROF_SAMPLE_END
    HeapBioProfSampleBegin {} -> EVENT_HEAP_BIO_PROF_SAMPLE_BEGIN
    HeapProfSampleCostCentre {} -> EVENT_HEAP_PROF_SAMPLE_COST_CENTRE
    HeapProfSampleString {} -> EVENT_HEAP_PROF_SAMPLE_STRING
    ProfSampleCostCentre {} -> EVENT_PROF_SAMPLE_COST_CENTRE
    ProfBegin {}            -> EVENT_PROF_BEGIN
    UserBinaryMessage {} -> EVENT_USER_BINARY_MSG
    ConcMarkBegin {} -> EVENT_CONC_MARK_BEGIN
    ConcMarkEnd {} -> EVENT_CONC_MARK_END
    ConcSyncBegin {} -> EVENT_CONC_SYNC_BEGIN
    ConcSyncEnd {} -> EVENT_CONC_SYNC_END
    ConcSweepBegin {} -> EVENT_CONC_SWEEP_BEGIN
    ConcSweepEnd {} -> EVENT_CONC_SWEEP_END
    ConcUpdRemSetFlush {} -> EVENT_CONC_UPD_REM_SET_FLUSH
    NonmovingHeapCensus {} -> EVENT_NONMOVING_HEAP_CENSUS
    TickyCounterDef {} -> EVENT_TICKY_COUNTER_DEF
    TickyCounterSample {} -> EVENT_TICKY_COUNTER_SAMPLE
    InfoTableProv {} -> EVENT_IPE
    MemReturn {} -> EVENT_MEM_RETURN
    TickyBeginSample {} -> EVENT_TICKY_BEGIN_SAMPLE

nEVENT_PERF_NAME, nEVENT_PERF_COUNTER, nEVENT_PERF_TRACEPOINT :: EventTypeNum
nEVENT_PERF_NAME = EVENT_PERF_NAME
nEVENT_PERF_COUNTER = EVENT_PERF_COUNTER
nEVENT_PERF_TRACEPOINT = EVENT_PERF_TRACEPOINT

putEvent :: Event -> PutM ()
putEvent Event {..} = do
    putType (eventTypeNum evSpec)
    put evTime
    putEventSpec evSpec

putEventSpec :: EventInfo -> PutM ()
putEventSpec (Startup caps) = do
    putCap (fromIntegral caps)

putEventSpec (EventBlock end cap sz) = do
    putE (fromIntegral (sz+24) :: BlockSize)
    putE end
    putE (fromIntegral cap :: CapNo)

putEventSpec (CreateThread t) =
    putE t

putEventSpec (RunThread t) =
    putE t

-- here we assume that ThreadStopStatus fromEnum matches the definitions in
-- EventLogFormat.h
-- The standard encoding is used here, which is wrong for event logs
-- produced by GHC-7.8.2 ([Stop status in GHC-7.8.2] in EventTypes.hs
putEventSpec (StopThread t s) = do
    putE t
    putE $ case s of
            NoStatus -> 0 :: Word16
            HeapOverflow -> 1
            StackOverflow -> 2
            ThreadYielding -> 3
            ThreadBlocked -> 4
            ThreadFinished -> 5
            ForeignCall -> 6
            BlockedOnMVar -> 7
            BlockedOnMVarRead -> 20 -- since GHC-7.8.3
            BlockedOnBlackHole -> 8
            BlockedOnBlackHoleOwnedBy _ -> 8
            BlockedOnRead -> 9
            BlockedOnWrite -> 10
            BlockedOnDelay -> 11
            BlockedOnSTM -> 12
            BlockedOnDoProc -> 13
            BlockedOnCCall -> 14
            BlockedOnCCall_NoUnblockExc -> 15
            BlockedOnMsgThrowTo -> 16
            ThreadMigrating -> 17
            BlockedOnMsgGlobalise -> 18
    putE $ case s of
            BlockedOnBlackHoleOwnedBy i -> i
            _                           -> 0

putEventSpec (ThreadRunnable t) =
    putE t

putEventSpec (MigrateThread t c) = do
    putE t
    putCap c

putEventSpec (CreateSparkThread t) =
    putE t

putEventSpec (SparkCounters crt dud ovf cnv fiz gcd rem) = do
    putE crt
    putE dud
    putE ovf
    putE cnv
    -- Warning: order of fiz and gcd reversed!
    putE gcd
    putE fiz
    putE rem

putEventSpec SparkCreate =
    return ()

putEventSpec SparkDud =
    return ()

putEventSpec SparkOverflow =
    return ()

putEventSpec SparkRun =
    return ()

putEventSpec (SparkSteal c) =
    putCap c

putEventSpec SparkFizzle =
    return ()

putEventSpec SparkGC =
    return ()

putEventSpec (WakeupThread t c) = do
    putE t
    putCap c

putEventSpec (ThreadLabel t (TE.encodeUtf8 -> l)) = do
    putE (fromIntegral (B.length l) + sz_tid :: Word16)
    putE t
    putByteString l

putEventSpec Shutdown =
    return ()

putEventSpec RequestSeqGC =
    return ()

putEventSpec RequestParGC =
    return ()

putEventSpec StartGC =
    return ()

putEventSpec GCWork =
    return ()

putEventSpec GCIdle =
    return ()

putEventSpec GCDone =
    return ()

putEventSpec EndGC =
    return ()

putEventSpec GlobalSyncGC =
    return ()

putEventSpec (TaskCreate taskId cap tid) = do
    putE taskId
    putCap cap
    putE tid

putEventSpec (TaskMigrate taskId cap new_cap) = do
    putE taskId
    putCap cap
    putCap new_cap

putEventSpec (TaskDelete taskId) =
    putE taskId

putEventSpec GCStatsGHC{..} = do
    putE heapCapset
    putE (fromIntegral gen :: Word16)
    putE copied
    putE slop
    putE frag
    putE (fromIntegral parNThreads :: Word32)
    putE parMaxCopied
    putE parTotCopied
    case parBalancedCopied of
      Nothing -> return ()
      Just v  -> putE v

putEventSpec MemReturn{..} = do
    putE heapCapset
    putE current
    putE needed
    putE returned

putEventSpec HeapAllocated{..} = do
    putE heapCapset
    putE allocBytes

putEventSpec HeapSize{..} = do
    putE heapCapset
    putE sizeBytes

putEventSpec BlocksSize{..} = do
    putE heapCapset
    putE blocksSize

putEventSpec HeapLive{..} = do
    putE heapCapset
    putE liveBytes

putEventSpec HeapInfoGHC{..} = do
    putE heapCapset
    putE (fromIntegral gens :: Word16)
    putE maxHeapSize
    putE allocAreaSize
    putE mblockSize
    putE blockSize

putEventSpec CapCreate{cap} =
    putCap cap

putEventSpec CapDelete{cap} =
    putCap cap

putEventSpec CapDisable{cap} =
    putCap cap

putEventSpec CapEnable{cap} =
    putCap cap

putEventSpec (CapsetCreate cs ct) = do
    putE cs
    putE $ case ct of
            CapsetCustom -> 1 :: Word16
            CapsetOsProcess -> 2
            CapsetClockDomain -> 3
            CapsetUnknown -> 0

putEventSpec (CapsetDelete cs) =
    putE cs

putEventSpec (CapsetAssignCap cs cp) = do
    putE cs
    putCap cp

putEventSpec (CapsetRemoveCap cs cp) = do
    putE cs
    putCap cp

putEventSpec (RtsIdentifier cs (TE.encodeUtf8 -> rts)) = do
    putE (fromIntegral (B.length rts) + sz_capset :: Word16)
    putE cs
    putByteString rts

putEventSpec (ProgramArgs cs (map TE.encodeUtf8 -> as)) = do
    let sz_args = sum (map ((+ 1) {- for \0 -} . B.length) as) - 1
    putE (fromIntegral sz_args + sz_capset :: Word16)
    putE cs
    mapM_ putByteString (intersperse "\0" as)

putEventSpec (ProgramEnv cs (map TE.encodeUtf8 -> es)) = do
    let sz_env = sum (map ((+ 1) {- for \0 -} . B.length) es) - 1
    putE (fromIntegral sz_env + sz_capset :: Word16)
    putE cs
    mapM_ putByteString $ intersperse "\0" es

putEventSpec (OsProcessPid cs pid) = do
    putE cs
    putE pid

putEventSpec (OsProcessParentPid cs ppid) = do
    putE cs
    putE ppid

putEventSpec (WallClockTime cs sec nsec) = do
    putE cs
    putE sec
    putE nsec

putEventSpec (Message (TE.encodeUtf8 -> s)) = do
    putE (fromIntegral (B.length s) :: Word16)
    putByteString s

putEventSpec (UserMessage (TE.encodeUtf8 -> s)) = do
    putE (fromIntegral (B.length s) :: Word16)
    putByteString s

putEventSpec (UserMarker (TE.encodeUtf8 -> s)) = do
    putE (fromIntegral (B.length s) :: Word16)
    putByteString s

putEventSpec (UnknownEvent {}) = error "putEventSpec UnknownEvent"

putEventSpec (InternString str id) = do
    putE len
    mapM_ putE str
    putE id
  where len = (fromIntegral (length str) :: Word16) + sz_string_id

putEventSpec (Version s) = do
    putE (fromIntegral (length s) :: Word16)
    mapM_ putE s

putEventSpec (ProgramInvocation s) = do
    putE (fromIntegral (length s) :: Word16)
    mapM_ putE s

putEventSpec ( EdenStartReceive ) = return ()

putEventSpec ( EdenEndReceive ) = return ()

putEventSpec ( CreateProcess  process ) = do
    putE process

putEventSpec ( KillProcess process ) = do
    putE process

putEventSpec ( AssignThreadToProcess thread process ) = do
    putE thread
    putE process

putEventSpec ( CreateMachine machine realtime ) = do
    putE machine
    putE realtime

putEventSpec ( KillMachine machine ) = do
    putE machine

putEventSpec ( SendMessage mesTag senderProcess senderThread
                 receiverMachine receiverProcess receiverInport ) = do
    putE (fromMsgTag mesTag)
    putE senderProcess
    putE senderThread
    putE receiverMachine
    putE receiverProcess
    putE receiverInport

putEventSpec ( ReceiveMessage mesTag receiverProcess receiverInport
                 senderMachine senderProcess senderThread messageSize ) = do
    putE (fromMsgTag mesTag)
    putE receiverProcess
    putE receiverInport
    putE senderMachine
    putE senderProcess
    putE senderThread
    putE messageSize

putEventSpec ( SendReceiveLocalMessage mesTag senderProcess senderThread
                 receiverProcess receiverInport ) = do
    putE (fromMsgTag mesTag)
    putE senderProcess
    putE senderThread
    putE receiverProcess
    putE receiverInport

putEventSpec (MerStartParConjunction dyn_id static_id) = do
    putE dyn_id
    putE static_id

putEventSpec (MerEndParConjunction dyn_id) =
    putE dyn_id

putEventSpec (MerEndParConjunct dyn_id) =
    putE dyn_id

putEventSpec (MerCreateSpark dyn_id spark_id) = do
    putE dyn_id
    putE spark_id

putEventSpec (MerFutureCreate future_id name_id) = do
    putE future_id
    putE name_id

putEventSpec (MerFutureWaitNosuspend future_id) =
    putE future_id

putEventSpec (MerFutureWaitSuspended future_id) =
    putE future_id

putEventSpec (MerFutureSignal future_id) =
    putE future_id

putEventSpec MerLookingForGlobalThread = return ()
putEventSpec MerWorkStealing = return ()
putEventSpec MerLookingForLocalSpark = return ()

putEventSpec (MerReleaseThread thread_id) =
    putE thread_id

putEventSpec MerCapSleeping = return ()
putEventSpec MerCallingMain = return ()

putEventSpec PerfName{name = (TE.encodeUtf8 -> name), ..} = do
    putE (fromIntegral (B.length name) + sz_perf_num :: Word16)
    putE perfNum
    putByteString name

putEventSpec PerfCounter{..} = do
    putE perfNum
    putE tid
    putE period

putEventSpec PerfTracepoint{..} = do
    putE perfNum
    putE tid

putEventSpec HeapProfBegin {..} = do
    putE heapProfId
    putE heapProfSamplingPeriod
    putE heapProfBreakdown
    mapM_ (putE . T.unpack)
      [ heapProfModuleFilter
      , heapProfClosureDescrFilter
      , heapProfTypeDescrFilter
      , heapProfCostCentreFilter
      , heapProfCostCentreStackFilter
      , heapProfRetainerFilter
      , heapProfBiographyFilter
      ]

putEventSpec HeapProfCostCentre {..} = do
    putE heapProfCostCentreId
    putE $ T.unpack heapProfLabel
    putE $ T.unpack heapProfModule
    putE $ T.unpack heapProfSrcLoc
    putE heapProfFlags

putEventSpec HeapProfSampleBegin {..} =
    putE heapProfSampleEra

putEventSpec HeapProfSampleEnd {..} =
    putE heapProfSampleEra

putEventSpec HeapBioProfSampleBegin {..} = do
    putE heapProfSampleEra
    putE heapProfSampleTime


putEventSpec HeapProfSampleCostCentre {..} = do
    putE heapProfId
    putE heapProfResidency
    putE heapProfStackDepth
    VU.mapM_ putE heapProfStack

putEventSpec HeapProfSampleString {..} = do
    putE heapProfId
    putE heapProfResidency
    putE $ T.unpack heapProfLabel

putEventSpec ProfSampleCostCentre {..} = do
    putE profCapset
    putE profTicks
    putE profStackDepth
    VU.mapM_ putE profCcsStack

putEventSpec ProfBegin {..} = do
    putE profTickInterval

putEventSpec UserBinaryMessage {..} = do
    putE (fromIntegral (B.length payload) :: Word16)
    putByteString payload

putEventSpec ConcMarkBegin = return ()
putEventSpec ConcMarkEnd {..} = do
    putE concMarkedObjectCount
putEventSpec ConcSyncBegin = return ()
putEventSpec ConcSyncEnd = return ()
putEventSpec ConcSweepBegin = return ()
putEventSpec ConcSweepEnd = return ()
putEventSpec ConcUpdRemSetFlush {..} = do
    putCap cap
putEventSpec NonmovingHeapCensus {..} = do
    putE nonmovingCensusBlkSize
    putE nonmovingCensusActiveSegs
    putE nonmovingCensusFilledSegs
    putE nonmovingCensusLiveBlocks
putEventSpec TickyCounterDef {..} = do
    putE tickyCtrDefId
    putE tickyCtrDefArity
    putE (T.unpack tickyCtrDefKinds)
    putE (T.unpack tickyCtrDefName)
    when (tickyCtrInfoTbl /= 0) $ do
      putE tickyCtrInfoTbl
      -- The json description without the info table field is not supported.
      when (isJust tickyCtrJsonDesc) $ putE (fromJust tickyCtrJsonDesc)
putEventSpec TickyCounterSample {..} = do
    putE tickyCtrSampleId
    putE tickyCtrSampleEntryCount
    putE tickyCtrSampleAllocs
    putE tickyCtrSampleAllocd
putEventSpec InfoTableProv{..} = do
    putE itInfo
    mapM_ (putE . T.unpack)
      [ itTableName
      , T.pack (show itClosureDesc)
      , itTyDesc
      , itLabel
      , itModule
      , itSrcLoc
      ]
putEventSpec TickyBeginSample = return ()
