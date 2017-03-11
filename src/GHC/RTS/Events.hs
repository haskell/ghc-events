{-# LANGUAGE CPP,BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -fsimpl-tick-factor=150 #-}
{-
 -   Parser functions for GHC RTS EventLog framework.
 -}

module GHC.RTS.Events (
       -- * The event log types
       EventLog(..),
       Header(..),
       Data(..),
       EventType(..),
       Event(..),
       EventInfo(..),
       ThreadStopStatus(..),
       CapsetType(..),
       Timestamp,
       ThreadId,
       TaskId,
       KernelThreadId(..),
       -- some types for the parallel RTS
       ProcessId,
       MachineId,
       PortId,
       MessageSize,
       MessageTag(..),

       -- * Reading and writing event logs
       readEventLogFromFile,
       writeEventLogToFile,

       -- * Utilities
       CapEvent(..), sortEvents,
       buildEventTypeMap,

       -- * Printing
       printEventsIncremental,
       showEventInfo, buildEventInfo,
       showThreadStopStatus,
       ppEventLog, ppEventType,
       ppEvent, buildEvent, buildEvent',

       -- * Perf events
       nEVENT_PERF_NAME, nEVENT_PERF_COUNTER, nEVENT_PERF_TRACEPOINT,
       sz_perf_num, sz_kernel_tid,

       -- * For compatibility with old clients
       -- readEventLogFromFile, TODO
       spec,
       time,
  ) where

{- Libraries. -}
import Control.Applicative
import Control.Concurrent hiding (ThreadId)
import qualified Data.Binary.Put as P
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Foldable (foldMap)
import Data.Function hiding (id)
import Data.List
import Data.Monoid ((<>))
import System.IO
import Prelude hiding (gcd, rem, id)

import GHC.RTS.EventTypes
import GHC.RTS.Events.Binary
import GHC.RTS.Events.Incremental

-- | Read an entire eventlog file. It returns an error message if it
-- encouters an error while decoding.
--
-- Note that it doesn't fail if it consumes all input in the middle of decoding
-- of an event.
readEventLogFromFile :: FilePath -> IO (Either String EventLog)
readEventLogFromFile path = fmap fst . readEventLog <$> BL.readFile path

-- | Read an eventlog file and pretty print it to stdout
printEventsIncremental
  :: Bool -- ^ Follow the file or not
  -> FilePath
  -> IO ()
printEventsIncremental follow path =
  withFile path ReadMode (hPrintEventsIncremental follow)

-- | Read an eventlog from the Handle and pretty print it to stdout
hPrintEventsIncremental
  :: Bool -- ^ Follow the handle or not
  -> Handle
  -> IO ()
hPrintEventsIncremental follow hdl = go decodeEventLog
  where
    go decoder = case decoder of
      Produce event decoder' -> do
        BB.hPutBuilder stdout $ buildEvent' event <> "\n"
        go decoder'
      Consume k -> do
        chunk <- B.hGetSome hdl 4096
        if
          | not (B.null chunk) -> go $ k chunk
          | follow -> threadDelay 1000000 >> go decoder
          | otherwise -> return ()
      Done {} -> return ()
      Error _ err -> fail err


-- | Writes the 'EventLog' to file. The log is expected to __NOT__ have 'EventBlock'
-- markers/events - the parsers no longer emit them and they are handled behind
-- the scenes.
writeEventLogToFile :: FilePath -> EventLog -> IO ()
writeEventLogToFile fp = BL.writeFile fp . serialiseEventLog


-- | Serialises an 'EventLog' back to a 'ByteString', usually for writing it
-- back to a file.
serialiseEventLog :: EventLog -> BL.ByteString
serialiseEventLog el@(EventLog _ (Data events)) =
  P.runPut $ putEventLog blockedEl
  where
    eventsMap = capSplitEvents events
    blockedEventsMap = IM.mapWithKey addBlockMarker eventsMap
    blockedEl = el{dat = Data blockedEvents}
    blockedEvents = IM.foldr (++) [] blockedEventsMap

-- Gets the Capability of an event in numeric form
getIntCap :: Event -> Int
getIntCap Event{evCap = cap} =
  case cap of
  Just capNo -> capNo
  Nothing    -> -1

-- Creates an IntMap of the events with capability number as the key.
-- Key -1 indicates global (capless) event
capSplitEvents :: [Event] -> IM.IntMap [Event]
capSplitEvents evts = capSplitEvents' evts IM.empty

capSplitEvents' :: [Event] -> IM.IntMap [Event] -> IM.IntMap [Event]
capSplitEvents' evts imap =
  case evts of
  (x:xs) -> capSplitEvents' xs (IM.insertWith (++) (getIntCap x) [x] imap)
  []     -> imap

-- Adds a block marker to the beginnng of a list of events, annotated with
-- its capability. All events are expected to belong to the same cap.
addBlockMarker :: Int -> [Event] -> [Event]
addBlockMarker cap evts =
  (Event startTime (EventBlock endTime cap sz) (mkCap cap)) : sortedEvts
  where
    sz = fromIntegral . BL.length $ P.runPut $ mapM_ putEvent evts
    startTime = case sortedEvts of
      (x:_) -> evTime x
      [] -> error "Cannot add block marker to an empty list of events"
    sortedEvts = sortEvents evts
    endTime = evTime $ last sortedEvts

-- -----------------------------------------------------------------------------
-- Utilities
sortEvents :: [Event] -> [Event]
sortEvents = sortBy (compare `on` evTime)

buildEventTypeMap :: [EventType] -> IntMap EventType
buildEventTypeMap etypes =
  IM.fromList [ (fromIntegral (num t),t) | t <- etypes ]

-----------------------------------------------------------------------------
-- Some pretty-printing support

showEventInfo :: EventInfo -> String
showEventInfo = BL8.unpack . BB.toLazyByteString . buildEventInfo

buildEventInfo :: EventInfo -> BB.Builder
buildEventInfo spec' =
    case spec' of
        EventBlock end_time cap _block_events ->
          "event block: cap " <> BB.intDec cap
          <> ", end time: " <> BB.word64Dec end_time <> "\n"
        Startup n_caps ->
          "startup: " <> BB.intDec n_caps <> " capabilities"
        CreateThread thread ->
          "creating thread " <> BB.word32Dec thread
        RunThread thread ->
          "running thread " <> BB.word32Dec thread
        StopThread thread status ->
          "stopping thread " <> BB.word32Dec thread
          <> " (" <> BB.stringUtf8 (showThreadStopStatus status) <> ")"
        ThreadRunnable thread ->
          "thread " <> BB.word32Dec thread <> " is runnable"
        MigrateThread thread newCap  ->
          "migrating thread " <> BB.word32Dec thread
          <> " to cap " <> BB.intDec newCap
        CreateSparkThread sparkThread ->
          "creating spark thread " <> BB.word32Dec sparkThread
        SparkCounters crt dud ovf cnv fiz gcd rem ->
          "spark stats: "
          <> BB.word64Dec crt <> " created, "
          <> BB.word64Dec cnv <> " converted, "
          <> BB.word64Dec rem <> " remaining ("
          <> BB.word64Dec ovf <> " overflowed, "
          <> BB.word64Dec dud <> " dud, "
          <> BB.word64Dec gcd <> " GC'd, "
          <> BB.word64Dec fiz <> " fizzled)"
        SparkCreate ->
          "spark created"
        SparkDud ->
          "dud spark discarded"
        SparkOverflow ->
          "overflowed spark discarded"
        SparkRun ->
          "running a local spark"
        SparkSteal victimCap ->
          "stealing a spark from cap " <> BB.intDec victimCap
        SparkFizzle ->
          "spark fizzled"
        SparkGC ->
          "spark GCed"
        TaskCreate taskId cap tid ->
          "task 0x" <> BB.word64Hex taskId
          <> " created on cap " <> BB.intDec cap
          <>" with OS kernel thread " <> BB.word64Dec (kernelThreadId tid)
        TaskMigrate taskId cap new_cap ->
          "task 0x" <> BB.word64Hex taskId
          <> " migrated from cap " <> BB.intDec cap
          <> " to cap " <> BB.intDec new_cap
        TaskDelete taskId ->
          "task 0x" <> BB.word64Hex taskId <> " deleted"
        Shutdown ->
          "shutting down"
        WakeupThread thread otherCap ->
          "waking up thread " <> BB.word32Dec thread
          <> " on cap " <> BB.intDec otherCap
        ThreadLabel thread label ->
          "thread " <> BB.word32Dec thread
          <> " has label \"" <> BB.stringUtf8 label <> "\""
        RequestSeqGC ->
          "requesting sequential GC"
        RequestParGC ->
          "requesting parallel GC"
        StartGC ->
          "starting GC"
        EndGC ->
          "finished GC"
        GCWork ->
          "GC working"
        GCIdle ->
          "GC idle"
        GCDone ->
          "GC done"
        GlobalSyncGC ->
          "all caps stopped for GC"
        GCStatsGHC{..} ->
          "GC stats for heap capset " <> BB.word32Dec heapCapset
          <> ": generation " <> BB.intDec gen <> ", "
          <> BB.word64Dec copied <> " bytes copied, "
          <> BB.word64Dec slop <> " bytes slop, "
          <> BB.word64Dec frag <> " bytes fragmentation, "
          <> BB.intDec parNThreads <> " par threads, "
          <> BB.word64Dec parMaxCopied <> " bytes max par copied, "
          <> BB.word64Dec parTotCopied <> " bytes total par copied"
        HeapAllocated{..} ->
          "allocated on heap capset " <> BB.word32Dec heapCapset
          <> ": " <> BB.word64Dec allocBytes <> " total bytes till now"
        HeapSize{..} ->
          "size of heap capset " <> BB.word32Dec heapCapset
          <> ": " <> BB.word64Dec sizeBytes <> " bytes"
        HeapLive{..} ->
          "live data in heap capset " <> BB.word32Dec heapCapset
          <> ": " <> BB.word64Dec liveBytes <> " bytes"
        HeapInfoGHC{..} ->
          "heap stats for heap capset " <> BB.word32Dec heapCapset
          <> ": generations " <> BB.intDec gens <> ", "
          <> BB.word64Dec maxHeapSize <> " bytes max heap size, "
          <> BB.word64Dec allocAreaSize <> " bytes alloc area size, "
          <> BB.word64Dec mblockSize <> " bytes mblock size, "
          <> BB.word64Dec blockSize <> " bytes block size"
        CapCreate{cap} ->
          "created cap " <> BB.intDec cap
        CapDelete{cap} ->
          "deleted cap " <> BB.intDec cap
        CapDisable{cap} ->
          "disabled cap " <> BB.intDec cap
        CapEnable{cap} ->
          "enabled cap " <> BB.intDec cap
        Message msg ->
          BB.stringUtf8 msg
        UserMessage msg ->
          BB.stringUtf8 msg
        UserMarker markername ->
          "marker: " <> BB.stringUtf8 markername
        CapsetCreate cs ct ->
          "created capset " <> BB.word32Dec cs
          <> " of type " <> BB.stringUtf8 (show ct)
        CapsetDelete cs ->
          "deleted capset " <> BB.word32Dec cs
        CapsetAssignCap cs cp ->
          "assigned cap " <> BB.intDec cp <> " to capset " <> BB.word32Dec cs
        CapsetRemoveCap cs cp ->
          "removed cap " <> BB.intDec cp <> " from capset " <> BB.word32Dec cs
        OsProcessPid cs pid ->
          "capset " <> BB.word32Dec cs <> ": pid " <> BB.word32Dec pid
        OsProcessParentPid cs ppid ->
          "capset " <> BB.word32Dec cs <> ": parent pid " <> BB.word32Dec ppid
        WallClockTime cs sec nsec ->
          "capset " <> BB.word32Dec cs <> ": wall clock time "
          <> BB.word64Dec sec <> "s "
          <> BB.word32Dec nsec <> "ns (unix epoch)"
        RtsIdentifier cs i ->
          "capset " <> BB.word32Dec cs
          <> ": RTS version \"" <> BB.stringUtf8 i <> "\""
        ProgramArgs cs args ->
          "capset " <> BB.word32Dec cs
          <> ": args: " <> BB.stringUtf8 (show args)
        ProgramEnv cs env ->
          "capset " <> BB.word32Dec cs
          <> ": env: " <> BB.stringUtf8 (show env)
        UnknownEvent n ->
          "Unknown event type " <> BB.word16Dec n
        InternString str sId ->
          "Interned string: \"" <> BB.stringUtf8 str
          <> "\" with id " <> BB.word32Dec sId
        -- events for the parallel RTS
        Version version ->
          "compiler version is " <> BB.stringUtf8 version
        ProgramInvocation  commandline ->
          "program invocation: " <> BB.stringUtf8 commandline
        EdenStartReceive ->
          "starting to receive"
        EdenEndReceive ->
          "stop receiving"
        CreateProcess  process ->
          "creating process " <> BB.word32Dec process
        KillProcess process ->
          "killing process " <> BB.word32Dec process
        AssignThreadToProcess thread process ->
          "assigning thread " <> BB.word32Dec thread
          <> " to process " <> BB.word32Dec process
        CreateMachine machine realtime ->
          "creating machine " <> BB.word16Dec machine
          <> " at " <> BB.word64Dec realtime
        KillMachine machine ->
          "killing machine " <> BB.word16Dec machine
        SendMessage mesTag senderProcess senderThread
          receiverMachine receiverProcess receiverInport ->
            "sending message with tag " <> BB.stringUtf8 (show mesTag)
            <> " from process " <> BB.word32Dec senderProcess
            <> ", thread " <> BB.word32Dec senderThread
            <> " to machine " <> BB.word16Dec receiverMachine
            <> ", process " <> BB.word32Dec receiverProcess
            <> " on inport " <> BB.word32Dec receiverInport
        ReceiveMessage mesTag receiverProcess receiverInport
          senderMachine senderProcess senderThread messageSize ->
            "receiving message with tag " <> BB.stringUtf8 (show mesTag)
            <> " at process " <> BB.word32Dec receiverProcess
            <> ", inport " <> BB.word32Dec receiverInport
            <> " from machine " <> BB.word16Dec senderMachine
            <> ", process " <> BB.word32Dec senderProcess
            <> ", thread " <> BB.word32Dec senderThread
            <> " with size " <> BB.word32Dec messageSize
        SendReceiveLocalMessage mesTag senderProcess senderThread
          receiverProcess receiverInport ->
            "sending/receiving message with tag " <> BB.stringUtf8 (show mesTag)
            <> " from process " <> BB.word32Dec senderProcess
            <> ", thread " <> BB.word32Dec senderThread
            <> " to process " <> BB.word32Dec receiverProcess
            <> " on inport " <> BB.word32Dec receiverInport
        MerStartParConjunction dyn_id static_id ->
          "Start a parallel conjunction 0x" <> BB.word64Hex dyn_id
          <> ", static_id: " <> BB.word32Dec static_id
        MerEndParConjunction dyn_id ->
          "End par conjunction: 0x" <> BB.word64Hex dyn_id
        MerEndParConjunct dyn_id ->
          "End par conjunct: 0x" <> BB.word64Hex dyn_id
        MerCreateSpark dyn_id spark_id ->
          "Create spark for conjunction: 0x" <> BB.word64Hex dyn_id
          <> " spark: 0x" <> BB.word32Hex spark_id
        MerFutureCreate future_id name_id ->
          "Create future 0x" <> BB.word64Hex future_id
          <> " named " <> BB.word32Dec name_id
        MerFutureWaitNosuspend future_id ->
          "Wait didn't suspend for future: 0x" <> BB.word64Hex future_id
        MerFutureWaitSuspended future_id ->
          "Wait suspended on future: 0x" <> BB.word64Hex future_id
        MerFutureSignal future_id ->
          "Signaled future 0x" <> BB.word64Hex future_id
        MerLookingForGlobalThread ->
          "Looking for global thread to resume"
        MerWorkStealing ->
          "Trying to steal a spark"
        MerLookingForLocalSpark ->
          "Looking for a local spark to execute"
        MerReleaseThread thread_id ->
          "Releasing thread " <> BB.word32Dec thread_id <> " to the free pool"
        MerCapSleeping ->
          "Capability going to sleep"
        MerCallingMain ->
          "About to call the program entry point"
        PerfName{perfNum, name} ->
          "perf event " <> BB.word32Dec perfNum
          <> " named \"" <> BB.stringUtf8 name <> "\""
        PerfCounter{perfNum, tid, period} ->
          "perf event counter " <> BB.word32Dec perfNum
          <> " incremented by " <> BB.word64Dec (period + 1)
          <> " in OS thread " <> BB.word64Dec (kernelThreadId tid)
        PerfTracepoint{perfNum, tid} ->
          "perf event tracepoint " <> BB.word32Dec perfNum
          <> " reached in OS thread " <> BB.word64Dec (kernelThreadId tid)

showThreadStopStatus :: ThreadStopStatus -> String
showThreadStopStatus HeapOverflow   = "heap overflow"
showThreadStopStatus StackOverflow  = "stack overflow"
showThreadStopStatus ThreadYielding = "thread yielding"
showThreadStopStatus ThreadBlocked  = "thread blocked"
showThreadStopStatus ThreadFinished = "thread finished"
showThreadStopStatus ForeignCall    = "making a foreign call"
showThreadStopStatus BlockedOnMVar  = "blocked on an MVar"
showThreadStopStatus BlockedOnMVarRead = "blocked reading an MVar"
showThreadStopStatus BlockedOnBlackHole = "blocked on a black hole"
showThreadStopStatus BlockedOnRead = "blocked on I/O read"
showThreadStopStatus BlockedOnWrite = "blocked on I/O write"
showThreadStopStatus BlockedOnDelay = "blocked on threadDelay"
showThreadStopStatus BlockedOnSTM = "blocked in STM retry"
showThreadStopStatus BlockedOnDoProc = "blocked on asyncDoProc"
showThreadStopStatus BlockedOnCCall = "blocked in a foreign call"
showThreadStopStatus BlockedOnCCall_NoUnblockExc = "blocked in a foreign call"
showThreadStopStatus BlockedOnMsgThrowTo = "blocked in throwTo"
showThreadStopStatus ThreadMigrating = "thread migrating"
showThreadStopStatus BlockedOnMsgGlobalise = "waiting for data to be globalised"
showThreadStopStatus (BlockedOnBlackHoleOwnedBy target) =
          "blocked on black hole owned by thread " ++ show target
showThreadStopStatus NoStatus = "No stop thread status"

ppEventLog :: EventLog -> String
ppEventLog = BL8.unpack . BB.toLazyByteString . buildEventLog

buildEventLog :: EventLog -> BB.Builder
buildEventLog (EventLog (Header ets) (Data es)) =
  "Event Types:\n"
  <> foldMap (\evType -> buildEventType evType <> "\n") ets
  <> "\n"
  <> "Events:\n"
  <> foldMap (\ev -> buildEvent imap ev <> "\n") sorted
  where
    imap = buildEventTypeMap ets
    sorted = sortEvents es

ppEventType :: EventType -> String
ppEventType = BL8.unpack . BB.toLazyByteString . buildEventType

buildEventType :: EventType -> BB.Builder
buildEventType (EventType num dsc msz) =
  BB.word16Dec num <> ": "
  <> BB.stringUtf8 dsc <> " (size "
  <> maybe "variable" BB.word16Dec msz <> ")"

-- | Pretty prints an 'Event', with clean handling for 'UnknownEvent'
ppEvent :: IntMap EventType -> Event -> String
ppEvent imap = BL8.unpack . BB.toLazyByteString . buildEvent imap

buildEvent :: IntMap EventType -> Event -> BB.Builder
buildEvent imap Event {..} =
  BB.word64Dec evTime
  <> ": "
  <> maybe "" (\c -> "cap " <> BB.intDec c <> ": ") evCap
  <> case evSpec of
    UnknownEvent{ ref=ref } ->
      maybe "" (BB.stringUtf8 . desc) $ IM.lookup (fromIntegral ref) imap
    _ -> buildEventInfo evSpec

buildEvent' :: Event -> BB.Builder
buildEvent' Event {..} =
   BB.word64Dec evTime
   <> ": "
   <> maybe "" (\c -> "cap " <> BB.intDec c <> ": ") evCap
   <> case evSpec of
     UnknownEvent{ ref=ref } ->
      "Unknown Event (ref: " <> BB.word16Dec ref <> ")"
     _ -> buildEventInfo evSpec
