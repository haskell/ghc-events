{-# LANGUAGE CPP #-}
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
       HeapProfBreakdown(..),
       HeapProfFlags(..),
       Timestamp,
       ThreadId,
       TaskId,
       KernelThreadId(..),
       EventTypeNum,
       EventTypeDesc,
       EventTypeSize,
       BlockSize,
       Capset,
       PID,
       StringId,
       -- some types for the parallel RTS
       ProcessId,
       MachineId,
       PortId,
       MessageSize,
       MessageTag(..),
       ParConjDynId,
       ParConjStaticId,
       SparkId,
       FutureId,
       PerfEventTypeNum,

       -- * Reading and writing event logs
       readEventLogFromFile,
       writeEventLogToFile,

       serialiseEventLog,

       -- * Utilities
       CapEvent(..), sortEvents,
       buildEventTypeMap,

       -- * Printing
       TimeFormat(..),
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
import qualified Data.ByteString.Lazy as BL
import Data.Char (isPrint)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Function hiding (id)
import Data.List
import Data.String (IsString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Vector.Unboxed as VU
import Data.Word
import System.IO
import Prelude hiding (gcd, rem, id)

import GHC.RTS.EventTypes
import GHC.RTS.Events.Binary
import GHC.RTS.Events.Incremental

#if !MIN_VERSION_base(4, 11, 0)
import Data.Monoid ((<>))
#endif

-- | Read an entire event log file. It returns an error message if it
-- encounters an error while decoding.
--
-- Note that it doesn't fail if it consumes all input in the middle of decoding
-- of an event.
readEventLogFromFile :: FilePath -> IO (Either String EventLog)
readEventLogFromFile path = fmap fst . readEventLog <$> BL.readFile path

-- | Read an event log file and pretty print it to stdout
printEventsIncremental
  :: Bool -- ^ Follow the file or not
  -> FilePath
  -> IO ()
printEventsIncremental follow path =
  withFile path ReadMode (hPrintEventsIncremental follow)

-- | Read an event log from the Handle and pretty print it to stdout
hPrintEventsIncremental
  :: Bool -- ^ Follow the handle or not
  -> Handle
  -> IO ()
hPrintEventsIncremental follow hdl = go decodeEventLog
  where
    go decoder = case decoder of
      Produce event decoder' -> do
        TL.hPutStrLn stdout $ TB.toLazyText $ buildEvent' event
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

-- Adds a block marker to the beginning of a list of events, annotated with
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
showEventInfo = TL.unpack . TB.toLazyText . buildEventInfo

buildEventInfo :: EventInfo -> TB.Builder
buildEventInfo spec' =
    case spec' of
        EventBlock end_time cap _block_events ->
          "event block: cap " <> TB.decimal cap
          <> ", end time: " <> TB.decimal end_time <> "\n"
        Startup n_caps ->
          "startup: " <> TB.decimal n_caps <> " capabilities"
        CreateThread thread ->
          "creating thread " <> TB.decimal thread
        RunThread thread ->
          "running thread " <> TB.decimal thread
        StopThread thread status ->
          "stopping thread " <> TB.decimal thread
          <> " (" <> TB.fromString (showThreadStopStatus status) <> ")"
        ThreadRunnable thread ->
          "thread " <> TB.decimal thread <> " is runnable"
        MigrateThread thread newCap  ->
          "migrating thread " <> TB.decimal thread
          <> " to cap " <> TB.decimal newCap
        CreateSparkThread sparkThread ->
          "creating spark thread " <> TB.decimal sparkThread
        SparkCounters crt dud ovf cnv fiz gcd rem ->
          "spark stats: "
          <> TB.decimal crt <> " created, "
          <> TB.decimal cnv <> " converted, "
          <> TB.decimal rem <> " remaining ("
          <> TB.decimal ovf <> " overflowed, "
          <> TB.decimal dud <> " dud, "
          <> TB.decimal gcd <> " GC'd, "
          <> TB.decimal fiz <> " fizzled)"
        SparkCreate ->
          "spark created"
        SparkDud ->
          "dud spark discarded"
        SparkOverflow ->
          "overflowed spark discarded"
        SparkRun ->
          "running a local spark"
        SparkSteal victimCap ->
          "stealing a spark from cap " <> TB.decimal victimCap
        SparkFizzle ->
          "spark fizzled"
        SparkGC ->
          "spark GCed"
        TaskCreate taskId cap tid ->
          "task 0x" <> TB.hexadecimal taskId
          <> " created on cap " <> TB.decimal cap
          <>" with OS kernel thread " <> TB.decimal (kernelThreadId tid)
        TaskMigrate taskId cap new_cap ->
          "task 0x" <> TB.hexadecimal taskId
          <> " migrated from cap " <> TB.decimal cap
          <> " to cap " <> TB.decimal new_cap
        TaskDelete taskId ->
          "task 0x" <> TB.hexadecimal taskId <> " deleted"
        Shutdown ->
          "shutting down"
        WakeupThread thread otherCap ->
          "waking up thread " <> TB.decimal thread
          <> " on cap " <> TB.decimal otherCap
        ThreadLabel thread label ->
          "thread " <> TB.decimal thread
          <> " has label \"" <> TB.fromText label <> "\""
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
          "GC stats for heap capset " <> TB.decimal heapCapset
          <> ": generation " <> TB.decimal gen <> ", "
          <> TB.decimal copied <> " bytes copied, "
          <> TB.decimal slop <> " bytes slop, "
          <> TB.decimal frag <> " bytes fragmentation, "
          <> TB.decimal parNThreads <> " par threads, "
          <> TB.decimal parMaxCopied <> " bytes max par copied, "
          <> TB.decimal parTotCopied <> " bytes total par copied"
          <> maybe mempty (\val -> ", " <> TB.decimal val <> " bytes balanced par copied") parBalancedCopied
        MemReturn{..} ->
          "memory returned (mblocks): current(" <> TB.decimal current  <>
                                   ") needed(" <> TB.decimal needed  <>
                                   ") returned(" <> TB.decimal returned <> ")"
        HeapAllocated{..} ->
          "allocated on heap capset " <> TB.decimal heapCapset
          <> ": " <> TB.decimal allocBytes <> " total bytes till now"
        HeapSize{..} ->
          "size of heap capset " <> TB.decimal heapCapset
          <> ": " <> TB.decimal sizeBytes <> " bytes"
        BlocksSize{..} ->
          "blocks size of heap capset " <> TB.decimal heapCapset
          <> ": " <> TB.decimal blocksSize <> " bytes"
        HeapLive{..} ->
          "live data in heap capset " <> TB.decimal heapCapset
          <> ": " <> TB.decimal liveBytes <> " bytes"
        HeapInfoGHC{..} ->
          "heap stats for heap capset " <> TB.decimal heapCapset
          <> ": generations " <> TB.decimal gens <> ", "
          <> TB.decimal maxHeapSize <> " bytes max heap size, "
          <> TB.decimal allocAreaSize <> " bytes alloc area size, "
          <> TB.decimal mblockSize <> " bytes mblock size, "
          <> TB.decimal blockSize <> " bytes block size"
        CapCreate{cap} ->
          "created cap " <> TB.decimal cap
        CapDelete{cap} ->
          "deleted cap " <> TB.decimal cap
        CapDisable{cap} ->
          "disabled cap " <> TB.decimal cap
        CapEnable{cap} ->
          "enabled cap " <> TB.decimal cap
        Message msg ->
          TB.fromText msg
        UserMessage msg ->
          TB.fromText msg
        UserMarker markername ->
          "marker: " <> TB.fromText markername
        CapsetCreate cs ct ->
          "created capset " <> TB.decimal cs
          <> " of type " <> TB.fromString (show ct)
        CapsetDelete cs ->
          "deleted capset " <> TB.decimal cs
        CapsetAssignCap cs cp ->
          "assigned cap " <> TB.decimal cp <> " to capset " <> TB.decimal cs
        CapsetRemoveCap cs cp ->
          "removed cap " <> TB.decimal cp <> " from capset " <> TB.decimal cs
        OsProcessPid cs pid ->
          "capset " <> TB.decimal cs <> ": pid " <> TB.decimal pid
        OsProcessParentPid cs ppid ->
          "capset " <> TB.decimal cs <> ": parent pid " <> TB.decimal ppid
        WallClockTime cs sec nsec ->
          "capset " <> TB.decimal cs <> ": wall clock time "
          <> TB.decimal sec <> "s "
          <> TB.decimal nsec <> "ns (unix epoch)"
        RtsIdentifier cs i ->
          "capset " <> TB.decimal cs
          <> ": RTS version \"" <> TB.fromText i <> "\""
        ProgramArgs cs args ->
          "capset " <> TB.decimal cs
          <> ": args: " <> TB.fromString (show args)
        ProgramEnv cs env ->
          "capset " <> TB.decimal cs
          <> ": env: " <> TB.fromString (show env)
        UnknownEvent n ->
          "Unknown event type " <> TB.decimal n
        InternString str sId ->
          "Interned string: \"" <> TB.fromString str
          <> "\" with id " <> TB.decimal sId
        -- events for the parallel RTS
        Version version ->
          "compiler version is " <> TB.fromString version
        ProgramInvocation  commandline ->
          "program invocation: " <> TB.fromString commandline
        EdenStartReceive ->
          "starting to receive"
        EdenEndReceive ->
          "stop receiving"
        CreateProcess  process ->
          "creating process " <> TB.decimal process
        KillProcess process ->
          "killing process " <> TB.decimal process
        AssignThreadToProcess thread process ->
          "assigning thread " <> TB.decimal thread
          <> " to process " <> TB.decimal process
        CreateMachine machine realtime ->
          "creating machine " <> TB.decimal machine
          <> " at " <> TB.decimal realtime
        KillMachine machine ->
          "killing machine " <> TB.decimal machine
        SendMessage mesTag senderProcess senderThread
          receiverMachine receiverProcess receiverInport ->
            "sending message with tag " <> TB.fromString (show mesTag)
            <> " from process " <> TB.decimal senderProcess
            <> ", thread " <> TB.decimal senderThread
            <> " to machine " <> TB.decimal receiverMachine
            <> ", process " <> TB.decimal receiverProcess
            <> " on inport " <> TB.decimal receiverInport
        ReceiveMessage mesTag receiverProcess receiverInport
          senderMachine senderProcess senderThread messageSize ->
            "receiving message with tag " <> TB.fromString (show mesTag)
            <> " at process " <> TB.decimal receiverProcess
            <> ", inport " <> TB.decimal receiverInport
            <> " from machine " <> TB.decimal senderMachine
            <> ", process " <> TB.decimal senderProcess
            <> ", thread " <> TB.decimal senderThread
            <> " with size " <> TB.decimal messageSize
        SendReceiveLocalMessage mesTag senderProcess senderThread
          receiverProcess receiverInport ->
            "sending/receiving message with tag " <> TB.fromString (show mesTag)
            <> " from process " <> TB.decimal senderProcess
            <> ", thread " <> TB.decimal senderThread
            <> " to process " <> TB.decimal receiverProcess
            <> " on inport " <> TB.decimal receiverInport
        MerStartParConjunction dyn_id static_id ->
          "Start a parallel conjunction 0x" <> TB.hexadecimal dyn_id
          <> ", static_id: " <> TB.decimal static_id
        MerEndParConjunction dyn_id ->
          "End par conjunction: 0x" <> TB.hexadecimal dyn_id
        MerEndParConjunct dyn_id ->
          "End par conjunct: 0x" <> TB.hexadecimal dyn_id
        MerCreateSpark dyn_id spark_id ->
          "Create spark for conjunction: 0x" <> TB.hexadecimal dyn_id
          <> " spark: 0x" <> TB.hexadecimal spark_id
        MerFutureCreate future_id name_id ->
          "Create future 0x" <> TB.hexadecimal future_id
          <> " named " <> TB.decimal name_id
        MerFutureWaitNosuspend future_id ->
          "Wait didn't suspend for future: 0x" <> TB.hexadecimal future_id
        MerFutureWaitSuspended future_id ->
          "Wait suspended on future: 0x" <> TB.hexadecimal future_id
        MerFutureSignal future_id ->
          "Signaled future 0x" <> TB.hexadecimal future_id
        MerLookingForGlobalThread ->
          "Looking for global thread to resume"
        MerWorkStealing ->
          "Trying to steal a spark"
        MerLookingForLocalSpark ->
          "Looking for a local spark to execute"
        MerReleaseThread thread_id ->
          "Releasing thread " <> TB.decimal thread_id <> " to the free pool"
        MerCapSleeping ->
          "Capability going to sleep"
        MerCallingMain ->
          "About to call the program entry point"
        PerfName{perfNum, name} ->
          "perf event " <> TB.decimal perfNum
          <> " named \"" <> TB.fromText name <> "\""
        PerfCounter{perfNum, tid, period} ->
          "perf event counter " <> TB.decimal perfNum
          <> " incremented by " <> TB.decimal (period + 1)
          <> " in OS thread " <> TB.decimal (kernelThreadId tid)
        PerfTracepoint{perfNum, tid} ->
          "perf event tracepoint " <> TB.decimal perfNum
          <> " reached in OS thread " <> TB.decimal (kernelThreadId tid)
        HeapProfBegin {..} ->
          "start heap profiling " <> TB.decimal heapProfId
          <> " at sampling period " <> TB.decimal heapProfSamplingPeriod
          <> " broken down by " <> showHeapProfBreakdown heapProfBreakdown
          <> maybe "" (" filtered by " <>)
            (buildFilters
              [ heapProfModuleFilter
              , heapProfClosureDescrFilter
              , heapProfTypeDescrFilter
              , heapProfCostCentreFilter
              , heapProfCostCentreStackFilter
              , heapProfRetainerFilter
              , heapProfBiographyFilter
              ])
        HeapProfCostCentre {..} ->
          "cost centre " <> TB.decimal heapProfCostCentreId
          <> " " <> TB.fromText heapProfLabel
          <> " in " <> TB.fromText heapProfModule
          <> " at " <> TB.fromText heapProfSrcLoc
          <> if isCaf heapProfFlags then " CAF" else ""
        InfoTableProv{..} ->
         "Info Table: " <> TB.hexadecimal itInfo <> ":"
                        <> TB.decimal itClosureDesc <> ":"
                        <> TB.fromText itTableName
                        <> " - " <> TB.fromText itSrcLoc
        HeapProfSampleBegin {..} ->
          "start heap prof sample " <> TB.decimal heapProfSampleEra
        HeapProfSampleEnd {..} ->
          "end prof sample " <> TB.decimal heapProfSampleEra
        HeapBioProfSampleBegin {..} ->
          "start heap prof sample " <> TB.decimal heapProfSampleEra
            <> " at time " <> TB.decimal heapProfSampleTime


        HeapProfSampleCostCentre {..} ->
          "heap prof sample " <> TB.decimal heapProfId
          <> ", residency " <> TB.decimal heapProfResidency
          <> ", cost centre stack " <> buildCostCentreStack heapProfStack

        HeapProfSampleString {..} ->
          "heap prof sample " <> TB.decimal heapProfId
          <> ", residency " <> TB.decimal heapProfResidency
          <> ", label " <> TB.fromText heapProfLabel

        ProfSampleCostCentre {..} ->
          "cap no " <> TB.decimal profCapset
          <> ", prof sample " <> TB.decimal profTicks
          <> ", cost centre stack " <> buildCostCentreStack profCcsStack

        ProfBegin {..} ->
          "start time profiling, tick interval " <> TB.decimal profTickInterval <> " (ns)"

        UserBinaryMessage {..} ->
          "binary message " <> TB.fromText (replaceUnprintableWith '.' payload)

        ConcMarkBegin    ->
          "concurrent mark began"
        ConcMarkEnd {..} ->
          "concurrent mark ended: "
          <> "marked " <> TB.decimal concMarkedObjectCount <> " objects"
        ConcSyncBegin ->
          "post-mark synchronization began"
        ConcSyncEnd ->
          "post-mark synchronization ended"
        ConcSweepBegin ->
          "concurrent sweep began"
        ConcSweepEnd ->
          "concurrent sweep ended"
        ConcUpdRemSetFlush {..}  ->
          "update remembered set flushed by " <> TB.decimal cap
        NonmovingHeapCensus {..}  ->
          "nonmoving heap census " <> TB.decimal nonmovingCensusBlkSize
          <> ": " <> TB.decimal nonmovingCensusActiveSegs <> " active segments"
          <> ", " <> TB.decimal nonmovingCensusFilledSegs <> " filled segments"
          <> ", " <> TB.decimal nonmovingCensusLiveBlocks <> " live blocks"
        TickyCounterDef {..}  ->
          "ticky counter definition " <> TB.decimal tickyCtrDefId
          <> ", " <>  "arity: " <> TB.decimal tickyCtrDefArity
          <> ", " <> "def kinds: " <> TB.fromText tickyCtrDefKinds
          <> ", " <> "name: " <> TB.fromText tickyCtrDefName
          <> ", " <> "itbl: " <> TB.hexadecimal tickyCtrInfoTbl
        TickyCounterSample {..}  ->
          "ticky counter sample " <> TB.decimal tickyCtrSampleId
          <> ": " <> "entry count: " <> TB.decimal tickyCtrSampleEntryCount
          <> ", " <> TB.decimal tickyCtrSampleAllocs <> " allocs"
          <> ", " <> TB.decimal tickyCtrSampleAllocd <> " allocd"
        TickyBeginSample ->
          "ticky begin counter sample"

-- | Replace unprintable bytes in the message with the replacement character
replaceUnprintableWith
  :: Char -- ^ Replacement character
  -> B.ByteString -- ^ Binary message which may contain unprintable bytes
  -> T.Text
replaceUnprintableWith replacement = T.map replace . TE.decodeUtf8With (\_ _ -> Just replacement)
  where
    replace c
      | isPrint c = c
      | otherwise = replacement

buildFilters :: [T.Text] -> Maybe TB.Builder
buildFilters = foldr g Nothing
  where
    g f b
      | T.null f = b
      | otherwise = Just (TB.fromText f <> ", ") <> b

buildCostCentreStack :: VU.Vector Word32 -> TB.Builder
buildCostCentreStack = VU.ifoldl' go ""
  where
    go b i cc
      | i == 0 = TB.decimal cc
      | otherwise = b <> ", " <> TB.decimal cc

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

showHeapProfBreakdown :: IsString s => HeapProfBreakdown -> s
showHeapProfBreakdown breakdown = case breakdown of
  HeapProfBreakdownCostCentre -> "cost centre"
  HeapProfBreakdownModule -> "module"
  HeapProfBreakdownClosureDescr -> "closure description"
  HeapProfBreakdownTypeDescr -> "type description"
  HeapProfBreakdownRetainer -> "retainer"
  HeapProfBreakdownBiography -> "biography"
  HeapProfBreakdownClosureType -> "closure type"
  HeapProfBreakdownInfoTable -> "info table"
  HeapProfBreakdownEra -> "era"

-- | How to format event timestamps
data TimeFormat = RawTime | PrettyTime

ppEventLog :: TimeFormat -> EventLog -> String
ppEventLog time_fmt = TL.unpack . TB.toLazyText . buildEventLog time_fmt

buildEventLog :: TimeFormat -> EventLog -> TB.Builder
buildEventLog time_fmt (EventLog (Header ets) (Data es)) =
  "Event Types:\n"
  <> foldMap (\evType -> buildEventType evType <> "\n") ets
  <> "\n"
  <> "Events:\n"
  <> foldMap (\ev -> buildEvent time_fmt imap ev <> "\n") sorted
  where
    imap = buildEventTypeMap ets
    sorted = sortEvents es

ppEventType :: EventType -> String
ppEventType = TL.unpack . TB.toLazyText . buildEventType

buildEventType :: EventType -> TB.Builder
buildEventType (EventType num dsc msz) =
  TB.decimal num <> ": "
  <> TB.fromText dsc <> " (size "
  <> maybe "variable" TB.decimal msz <> ")"

-- | Pretty prints an 'Event', with clean handling for 'UnknownEvent'
ppEvent
  :: TimeFormat
  -> IntMap EventType -- ^ Look up @'UnknownEvent'.'ref'@ to find a suitable description.
  -> Event
  -> String
ppEvent time_fmt imap =
  TL.unpack . TB.toLazyText . buildEvent time_fmt imap

buildEvent :: TimeFormat -> IntMap EventType -> Event -> TB.Builder
buildEvent time_fmt imap Event {..} =
  buildTime time_fmt evTime
  <> ": "
  <> maybe "" (\c -> "cap " <> TB.decimal c <> ": ") evCap
  <> case evSpec of
    UnknownEvent{ ref=ref } ->
      maybe "" (TB.fromText . desc) $ IM.lookup (fromIntegral ref) imap
    _ -> buildEventInfo evSpec

buildTime :: TimeFormat -> Word64 -> TB.Builder
buildTime RawTime t = TB.decimal t
buildTime PrettyTime t =
     rJustDecimal secs  <> "s "
  <> rJustDecimal msecs <> "ms "
  <> rJustDecimal usecs
  <> "."
  <> lJustDecimal nsecs''  <> "us"
  where
    (secs, nsecs) = t `divMod` (1000*1000*1000)
    (msecs, nsecs') = nsecs `divMod` (1000*1000)
    (usecs, nsecs'') = nsecs' `divMod` 1000

    padSpaces :: Word64 -> TB.Builder
    padSpaces n
      | n < 10    = "  "
      | n < 100   = " "
      | otherwise = mempty

    -- decimal numbers justified to three columns
    rJustDecimal n = padSpaces n <> TB.decimal n
    lJustDecimal n = TB.decimal n <> padSpaces n

buildEvent' :: Event -> TB.Builder
buildEvent' Event {..} =
   TB.decimal evTime
   <> ": "
   <> maybe "" (\c -> "cap " <> TB.decimal c <> ": ") evCap
   <> case evSpec of
     UnknownEvent{ ref=ref } ->
      "Unknown Event (ref: " <> TB.decimal ref <> ")"
     _ -> buildEventInfo evSpec
