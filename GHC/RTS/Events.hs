{-# LANGUAGE CPP,BangPatterns,PatternGuards #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-
 - Author: Donnie Jones, Simon Marlow
 - Events.hs
 -   Parser functions for GHC RTS EventLog framework.
 -}
 
module GHC.RTS.Events (
       -- * The event log types                       
       EventLog(..),
       EventType(..),
       Event(..),
       EventTypeSpecificInfo(..),
       ThreadStopStatus(..),
       Header(..),
       Data(..),
       CapsetType(..),
       Timestamp,
       ThreadId,

       -- * Reading and writing event logs
       readEventLogFromFile,
       writeEventLogToFile,

       -- * Utilities
       CapEvent(..), sortEvents, groupEvents, sortGroups,
       buildEventTypeMap,

       -- * Printing
       showEventTypeSpecificInfo, showThreadStopStatus,
       ppEventLog, ppEventType, ppEvent
  ) where

{- Libraries. -}
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Control.Monad
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Control.Monad.Reader
import Control.Monad.Error
import qualified Data.ByteString.Lazy as L
import Data.Function
import Data.List
import Data.Either
import Data.Maybe
import Text.Printf
import Data.Array

import GHC.RTS.EventTypes
import GHC.RTS.EventParserUtils

#define EVENTLOG_CONSTANTS_ONLY
#include "EventLogFormat.h"

------------------------------------------------------------------------------
-- Binary instances

getEventType :: GetHeader EventType
getEventType = do 
           etNum <- getH
           size <- getH :: GetHeader EventTypeSize
           let etSize = if size == 0xffff then Nothing else Just size
           -- 0xffff indicates variable-sized event
           etDescLen <- getH :: GetHeader EventTypeDescLen
           etDesc <- getEtDesc (fromIntegral etDescLen) 
           etExtraLen <- getH :: GetHeader Word32
           _skip  <- replicateM_ (fromIntegral etExtraLen) (lift getWord8)
           ete <- getH :: GetHeader Marker
           when (ete /= EVENT_ET_END) $
              throwError ("Event Type end marker not found.")
           return (EventType etNum etDesc etSize)
           where 
             getEtDesc :: Int -> GetHeader [Char]
             getEtDesc s = replicateM s (getH :: GetHeader Char)

getHeader :: GetHeader Header
getHeader = do 
           hdrb <- getH :: GetHeader Marker
           when (hdrb /= EVENT_HEADER_BEGIN) $
                throwError "Header begin marker not found"
           hetm <- getH :: GetHeader Marker
           when (hetm /= EVENT_HET_BEGIN) $ 
                throwError "Header Event Type begin marker not found"
           ets <- getEventTypes
           emark <- getH :: GetHeader Marker
           when (emark /= EVENT_HEADER_END) $
                throwError "Header end marker not found"
           return (Header ets)
     where    
       getEventTypes :: GetHeader [EventType]
       getEventTypes = do
           m <- getH :: GetHeader Marker
           case () of 
            _ | m == EVENT_ET_BEGIN -> do
                   et <- getEventType
                   nextET <- getEventTypes
                   return (et : nextET)
              | m == EVENT_HET_END ->
                   return []
              | otherwise ->
                   throwError "Malformed list of Event Types in header"

getEvent :: EventParsers -> GetEvents (Maybe Event)
getEvent (EventParsers parsers) = do 
  etRef <- getE :: GetEvents EventTypeNum
  if (etRef == EVENT_DATA_END) 
     then return Nothing
     else do !ts   <- getE
             -- trace ("event: " ++ show etRef) $ do
             spec <- parsers ! fromIntegral etRef
             return (Just (Event ts spec))

-- Our event log format allows new fields to be added to events over
-- time.  This means that our parser must be able to handle:
--
--  * old versions of an event, with fewer fields than expected,
--  * new versions of an event, with more fields than expected
--
-- The event log file declares the size for each event type, so we can
-- select the correct parser for the event type based on its size.  We
-- do this once after parsing the header: given the EventTypes, we build 
-- an array of event parsers indexed by event type.
-- 
-- For each event type, we may have multiple parsers for different
-- versions of the event, indexed by size.  These are listed in the
-- eventTypeParsers list below.  For the given log file we select the
-- parser for the most recent version (largest size less than the size
-- declared in the header).  If this is a newer version of the event
-- than we understand, there may be extra bytes that we have to read
-- and discard in the parser for this event type.
--
-- Summary:
--   if size is smaller that we expect:
--     parse the earier version, or ignore the event
--   if size is just right:
--     parse it
--   if size is too big:
--     parse the bits we understand and discard the rest

mkEventTypeParsers :: IntMap EventType
                   -> Array Int (GetEvents EventTypeSpecificInfo)
mkEventTypeParsers etypes 
 = accumArray (flip const) undefined (0, max_event_num)
    ([ (num, undeclared_etype num) | num <- [0..max_event_num] ] ++
     [ (num, parser num etype) | (num, etype) <- M.toList etypes ])
  where
    max_event_num = maximum (M.keys etypes)
    undeclared_etype num = throwError ("undeclared event type: " ++ show num)

    parser num etype =
         let
             possible
               | not (inRange (bounds eventTypeParsers) num) = []
               | otherwise = eventTypeParsers ! num
             mb_et_size = size etype
         in
         case mb_et_size of
           Nothing -> case M.lookup num variableEventTypeParsers of
                        Nothing -> noEventTypeParser num mb_et_size
                        Just p  -> p

           -- special case for GHC 6.12 EVENT_STOP_THREAD.  GHC 6.12
           -- was mis-reporting the event sizes (ThreadIds were
           -- counted as 8 instead of 4), and when we expanded the
           -- EVENT_STOP_THREAD to include an extra field, the new
           -- size is the same as that reported by 6.12, so we can't
           -- tell them apart by size.  Hence the special case here
           -- checks the size of the EVENT_CREATE_THREAD event to see
           -- whether we should be parsing the 6.12 STOP_THREAD or the
           -- 7.2 STOP_THREAD.  If the CREATE_THREAD extended in the
           -- future this might go wrong.

           Just et_size
             | et_size == sz_old_tid + 2,
               num == EVENT_STOP_THREAD,
                Just et <- M.lookup EVENT_CREATE_THREAD etypes,
                size et == Just sz_old_tid ->
                do  -- (thread, status)
                  t <- getE
                  s <- getE :: GetEvents Word16
                  let stat = fromIntegral s
                  return StopThread{thread=t, status = if stat > maxBound
                                                          then NoStatus
                                                          else mkStopStatus stat}

           Just et_size ->
             case [ (sz,p) | (sz,p) <- possible, sz <= et_size ] of
               [] -> noEventTypeParser num mb_et_size
               ps -> let (sz, best) = maximumBy (compare `on` fst) ps
                     in  if sz == et_size
                            then best
                            else do r <- best
                                    lift . lift $ 
                                      replicateM_ (fromIntegral (et_size - sz))
                                                getWord8
                                    return r


eventTypeParsers :: Array Int [(EventTypeSize, GetEvents EventTypeSpecificInfo)]
eventTypeParsers = accumArray (flip (:)) [] (0,NUM_EVENT_TAGS) [

 (EVENT_STARTUP, 
  (sz_cap, do -- (n_caps)
      c <- getE :: GetEvents CapNo
      return Startup{ n_caps = fromIntegral c }
   )),

 (EVENT_BLOCK_MARKER,
  (4 + sz_time + sz_cap, do -- (size, end_time, cap)
      block_size <- getE :: GetEvents Word32
      end_time <- getE :: GetEvents Timestamp
      c <- getE :: GetEvents CapNo
      lbs <- lift . lift $ getLazyByteString (fromIntegral block_size - 24)
      eparsers <- ask
      let e_events = runGet (runErrorT $ runReaderT (getEventBlock eparsers) eparsers) lbs
      return EventBlock{ end_time=end_time,
                         cap= fromIntegral c, 
                         block_events=case e_events of
                                        Left s -> error s
                                        Right es -> es }
   )),

 (EVENT_CREATE_THREAD,
  (sz_tid, do  -- (thread)
      t <- getE
      return CreateThread{thread=t}
   )),

 (EVENT_RUN_THREAD,
  (sz_tid, do  --  (thread)
      t <- getE
      return RunThread{thread=t}
   )),

 (EVENT_STOP_THREAD,
  (sz_tid + 2, do  -- (thread, status)
      t <- getE
      s <- getE :: GetEvents Word16
      let stat = fromIntegral s
      return StopThread{thread=t, status = if stat > maxBound
                                              then NoStatus
                                              else mkStopStatus stat}
   )),

 (EVENT_STOP_THREAD,
  (sz_tid + 2 + sz_tid, do  -- (thread, status, info)
      t <- getE
      s <- getE :: GetEvents Word16
      i <- getE :: GetEvents ThreadId
      let stat = fromIntegral s
      return StopThread{thread = t,
                        status = case () of
                                  _ | stat > maxStat
                                    -> NoStatus
                                    | stat == 8 {- XXX yeuch -}
                                    -> BlockedOnBlackHoleOwnedBy i
                                    | otherwise
                                    -> mkStopStatus stat}
   )),

 (EVENT_THREAD_RUNNABLE,
  (sz_tid, do  -- (thread)
      t <- getE
      return ThreadRunnable{thread=t}
   )),

 (EVENT_MIGRATE_THREAD,
  (sz_tid + sz_cap, do  --  (thread, newCap)
      t  <- getE
      nc <- getE :: GetEvents CapNo
      return MigrateThread{thread=t,newCap=fromIntegral nc}
   )),

 (EVENT_RUN_SPARK,
  (sz_tid, do  -- (thread)
      t <- getE
      return RunSpark{thread=t}
   )),

 (EVENT_STEAL_SPARK,
  (sz_tid + sz_cap, do  -- (thread, victimCap)
      t  <- getE
      vc <- getE :: GetEvents CapNo
      return StealSpark{thread=t,victimCap=fromIntegral vc}
   )),

 (EVENT_CREATE_SPARK_THREAD,
  (sz_tid, do  -- (sparkThread)
      st <- getE :: GetEvents ThreadId
      return CreateSparkThread{sparkThread=st}
   )),

 (EVENT_SHUTDOWN, (0, return Shutdown)),

 (EVENT_THREAD_WAKEUP,
  (sz_tid + sz_cap, do  -- (thread, other_cap)
      t <- getE
      oc <- getE :: GetEvents CapNo
      return WakeupThread{thread=t,otherCap=fromIntegral oc}
   )),

 (EVENT_REQUEST_SEQ_GC, (0, return RequestSeqGC)),

 (EVENT_REQUEST_PAR_GC, (0, return RequestParGC)),

 (EVENT_GC_START, (0, return StartGC)),

 (EVENT_GC_WORK, (0, return GCWork)),

 (EVENT_GC_IDLE, (0, return GCIdle)),

 (EVENT_GC_DONE, (0, return GCDone)),

 (EVENT_GC_END, (0, return EndGC)),

 -----------------------
 -- GHC 6.12 compat: GHC 6.12 reported the wrong sizes for some events,
 -- so we have to recognise those wrong sizes here for backwards 
 -- compatibility.

 (EVENT_STARTUP, 
  (0, do -- BUG in GHC 6.12: the startup event was incorrectly 
         -- declared as size 0, so we accept it here.
      c <- getE :: GetEvents CapNo
      return Startup{ n_caps = fromIntegral c }
   )),

 (EVENT_CREATE_THREAD,
  (sz_old_tid, do  -- (thread)
      t <- getE
      return CreateThread{thread=t}
   )),

 (EVENT_RUN_THREAD,
  (sz_old_tid, do  --  (thread)
      t <- getE
      return RunThread{thread=t}
   )),

 {-
 -- XXX this one doesn't work; see mkEventTypeParsers above
 (EVENT_STOP_THREAD,
  (sz_old_tid + 2, do  -- (thread, status)
      t <- getE
      s <- getE :: GetEvents Word16
      let stat = fromIntegral s
      return StopThread{thread=t, status = if stat > maxBound
                                              then NoStatus
                                              else mkStat stat}
   )),
 -}

 (EVENT_THREAD_RUNNABLE,
  (sz_old_tid, do  -- (thread)
      t <- getE
      return ThreadRunnable{thread=t}
   )),

 (EVENT_MIGRATE_THREAD,
  (sz_old_tid + sz_cap, do  --  (thread, newCap)
      t  <- getE
      nc <- getE :: GetEvents CapNo
      return MigrateThread{thread=t,newCap=fromIntegral nc}
   )),

 (EVENT_RUN_SPARK,
  (sz_old_tid, do  -- (thread)
      t <- getE
      return RunSpark{thread=t}
   )),

 (EVENT_STEAL_SPARK,
  (sz_old_tid + sz_cap, do  -- (thread, victimCap)
      t  <- getE
      vc <- getE :: GetEvents CapNo
      return StealSpark{thread=t,victimCap=fromIntegral vc}
   )),

 (EVENT_CREATE_SPARK_THREAD,
  (sz_old_tid, do  -- (sparkThread)
      st <- getE :: GetEvents ThreadId
      return CreateSparkThread{sparkThread=st}
   )),

 (EVENT_THREAD_WAKEUP,
  (sz_old_tid + sz_cap, do  -- (thread, other_cap)
      t <- getE
      oc <- getE :: GetEvents CapNo
      return WakeupThread{thread=t,otherCap=fromIntegral oc}
   )),
 (EVENT_CAPSET_CREATE,
  (sz_capset + sz_capset_type, do -- (capset, capset_type)
      cs <- getE
      ct <- fmap mkCapsetType getE
      return CapsetCreate{capset=cs,capsetType=ct}
   )),
 (EVENT_CAPSET_DELETE,
  (sz_capset, do -- (capset)
      cs <- getE
      return CapsetDelete{capset=cs}
   )),
 (EVENT_CAPSET_ASSIGN_CAP,
  (sz_capset + sz_cap, do -- (capset, cap)
      cs <- getE
      cp <- getE :: GetEvents CapNo
      return CapsetAssignCap{capset=cs,cap=fromIntegral cp}
   )),
 (EVENT_CAPSET_REMOVE_CAP,
  (sz_capset + sz_cap, do -- (capset, cap)
      cs <- getE
      cp <- getE :: GetEvents CapNo
      return CapsetRemoveCap{capset=cs,cap=fromIntegral cp}
   )),
 (EVENT_OSPROCESS_PID,
  (sz_capset + 4, do -- (capset, pid)
      cs <- getE
      pd <- getE
      return OsProcessPid{capset=cs,pid=pd}
   )),
 (EVENT_OSPROCESS_PPID,
  (sz_capset + 4, do -- (capset, ppid)
      cs <- getE
      pd <- getE
      return OsProcessParentPid{capset=cs,ppid=pd}
  ))
 ]

variableEventTypeParsers :: IntMap (GetEvents EventTypeSpecificInfo)
variableEventTypeParsers = M.fromList [

 (EVENT_LOG_MSG, do -- (msg)
      num <- getE :: GetEvents Word16
      string <- getString num
      return Message{ msg = string }
   ),

 (EVENT_USER_MSG, do -- (msg)
      num <- getE :: GetEvents Word16
      string <- getString num
      return UserMessage{ msg = string }
   ),
 (EVENT_PROGRAM_ARGS, do -- (capset, [arg])
      num <- getE :: GetEvents Word16
      cs <- getE
      string <- getString (num - 4)
      return ProgramArgs{ capset = cs
                        , args = splitNull string }
   ),
 (EVENT_PROGRAM_ENV, do -- (capset, [arg])
      num <- getE :: GetEvents Word16
      cs <- getE
      string <- getString (num - 4)
      return ProgramEnv{ capset = cs
                       , env = splitNull string }
   ),
 (EVENT_RTS_IDENTIFIER, do -- (capset, str)
      num <- getE :: GetEvents Word16
      cs <- getE
      string <- getString (num - 4)
      return RtsIdentifier{ capset = cs
                          , rtsident = string }
   )
 ]
 where
    splitNull [] = []
    splitNull xs = case span (/= '\0') xs of
                    (x, xs') -> x : splitNull (drop 1 xs')


noEventTypeParser :: Int -> Maybe EventTypeSize
                  -> GetEvents EventTypeSpecificInfo
noEventTypeParser num mb_size = do
  bytes <- case mb_size of
             Just n  -> return n
             Nothing -> getE :: GetEvents Word16
  skip  <- lift . lift $ replicateM_ (fromIntegral bytes) getWord8
  return UnknownEvent{ ref = fromIntegral num }



getData :: GetEvents Data
getData = do
   db <- getE :: GetEvents Marker
   when (db /= EVENT_DATA_BEGIN) $ throwError "Data begin marker not found"
   eparsers <- ask
   let 
       getEvents :: [Event] -> GetEvents Data
       getEvents events = do
         mb_e <- getEvent eparsers
         case mb_e of
           Nothing -> return (Data (reverse events))
           Just e  -> getEvents (e:events)
   -- in
   getEvents []

getEventBlock :: EventParsers -> GetEvents [Event]
getEventBlock parsers = do
  b <- lift . lift $ isEmpty
  if b then return [] else do
  mb_e <- getEvent parsers
  case mb_e of
    Nothing -> return []
    Just e  -> do
      es <- getEventBlock parsers
      return (e:es)

getEventLog :: ErrorT String Get EventLog
getEventLog = do
    header <- getHeader
    let imap = M.fromList [ (fromIntegral (num t),t) | t <- eventTypes header]
        parsers = mkEventTypeParsers imap
    dat <- runReaderT getData (EventParsers parsers)
    return (EventLog header dat)

readEventLogFromFile :: FilePath -> IO (Either String EventLog)
readEventLogFromFile f = do
    s <- L.readFile f
    return $ runGet (do v <- runErrorT $ getEventLog
                        m <- isEmpty
                        m `seq` return v)  s

-- -----------------------------------------------------------------------------
-- Utilities

sortEvents :: [Event] -> [CapEvent]
sortEvents = sortGroups . groupEvents

-- | Sort the raw event stream by time, annotating each event with the
-- capability that generated it.
sortGroups :: [(Maybe Int, [Event])] -> [CapEvent]
sortGroups groups = mergesort' (compare `on` (time . ce_event)) $
                      [ [ CapEvent cap e | e <- es ] 
                      | (cap, es) <- groups ]
     -- sorting is made much faster by the way that the event stream is
     -- divided into blocks of events.  
     --  - All events in a block belong to a particular capability
     --  - The events in a block are ordered by time
     --  - blocks for the same capability appear in time order in the event
     --    stream and do not overlap.
     --
     -- So to sort the events we make one list of events for each
     -- capability (basically just concat . filter), and then
     -- merge the resulting lists.

groupEvents :: [Event] -> [(Maybe Int, [Event])]
groupEvents es = (Nothing, n_events) : 
                 [ (Just (cap (head blocks)), concatMap block_events blocks)
                 | blocks <- groups ]
  where
   (blocks, anon_events) = partitionEithers (map separate es)
      where separate e | b@EventBlock{} <- spec e = Left  b
                       | otherwise                = Right e

   (cap_blocks, gbl_blocks) = partition (is_cap . cap) blocks
      where is_cap c = fromIntegral c /= ((-1) :: Word16)

   groups = groupBy ((==) `on` cap) $ sortBy (compare `on` cap) cap_blocks

     -- There are two sources of events without a capability: events
     -- in the raw stream not inside an EventBlock, and EventBlocks
     -- with cap == -1.  We have to merge those two streams.
     -- In light of merged logs, global blocks may have overlapping
     -- time spans, thus the blocks are mergesorted
   n_events = mergesort' (compare `on` time) (anon_events : map block_events gbl_blocks)

mergesort' :: (a -> a -> Ordering) -> [[a]] -> [a]
mergesort' _   [] = []
mergesort' _   [xs] = xs
mergesort' cmp xss = mergesort' cmp (merge_pairs cmp xss)

merge_pairs :: (a -> a -> Ordering) -> [[a]] -> [[a]]
merge_pairs _   [] = []
merge_pairs _   [xs] = [xs]
merge_pairs cmp (xs:ys:xss) = merge cmp xs ys : merge_pairs cmp xss

merge :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
merge _   [] ys = ys
merge _   xs [] = xs
merge cmp (x:xs) (y:ys)
 = case x `cmp` y of
        GT -> y : merge cmp (x:xs)   ys
        _  -> x : merge cmp    xs (y:ys)


buildEventTypeMap :: [EventType] -> IntMap EventType
buildEventTypeMap etypes = M.fromList [ (fromIntegral (num t),t) | t <- etypes ]

-----------------------------------------------------------------------------
-- Some pretty-printing support

showEventTypeSpecificInfo :: EventTypeSpecificInfo -> String
showEventTypeSpecificInfo spec =
      case spec of
        Startup n_caps ->
          printf "startup: %d capabilities" n_caps
        EventBlock end_time cap _block_events ->
          printf "event block: cap %d, end time: %d\n" cap end_time
        CreateThread thread ->
          printf "creating thread %d" thread
        RunThread thread ->
          printf "running thread %d" thread
        StopThread thread status ->
          printf "stopping thread %d (%s)" thread (showThreadStopStatus status)
        ThreadRunnable thread ->
          printf "thread %d is runnable" thread
        MigrateThread thread newCap  ->
          printf "migrating thread %d to cap %d" thread newCap
        RunSpark thread ->
          printf "running a local spark (thread %d)" thread
        StealSpark thread victimCap ->
          printf "thread %d stealing a spark from cap %d" thread victimCap
        CreateSparkThread sparkThread ->
          printf "creating spark thread %d" sparkThread
        Shutdown ->
          printf "shutting down"
        WakeupThread thread otherCap ->
          printf "waking up thread %d on cap %d" thread otherCap
        RequestSeqGC ->
          printf "requesting sequential GC"
        RequestParGC ->
          printf "requesting parallel GC"
        StartGC ->
          printf "starting GC"
        EndGC ->
          printf "finished GC"
        GCWork ->
          printf "GC working"
        GCIdle ->
          printf "GC idle"
        GCDone ->
          printf "GC done"
        Message msg ->
          msg
        UserMessage msg ->
          msg
        CapsetCreate cs ct ->
          printf "created capset %d of type %s" cs (show ct)
        CapsetDelete cs ->
          printf "deleted capset %d" cs
        CapsetAssignCap cs cp ->
          printf "assigned cap %d to capset %d" cp cs
        CapsetRemoveCap cs cp ->
          printf "removed cap %d from capset %d" cp cs
        OsProcessPid cs pid ->
          printf "capset %d: pid %d" cs pid
        OsProcessParentPid cs ppid ->
          printf "capset %d: parent pid %d" cs ppid
        RtsIdentifier cs i ->
          printf "capset %d: RTS version %s" cs i
        ProgramArgs cs args ->
          printf "capset %d: args: %s" cs (show args)
        ProgramEnv cs env ->
          printf "capset %d: env: %s" cs (show env)
        UnknownEvent _ ->
          "Unknown event type"

showThreadStopStatus :: ThreadStopStatus -> String
showThreadStopStatus HeapOverflow   = "heap overflow"
showThreadStopStatus StackOverflow  = "stack overflow"
showThreadStopStatus ThreadYielding = "thread yielding"
showThreadStopStatus ThreadBlocked  = "thread blocked"
showThreadStopStatus ThreadFinished = "thread finished"
showThreadStopStatus ForeignCall    = "making a foreign call"
showThreadStopStatus BlockedOnMVar  = "blocked on an MVar"
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

ppEventLog :: EventLog -> String
ppEventLog (EventLog (Header ets) (Data es)) = unlines $ concat (
    [ ["Event Types:"]
    , map ppEventType ets
    , [""] -- newline
    , ["Events:"]
    , map (ppEvent imap) sorted
    , [""] ]) -- extra trailing newline
 where
    imap = buildEventTypeMap ets
    sorted = sortEvents es

ppEventType :: EventType -> String
ppEventType (EventType num dsc msz) = printf "%4d: %s (size %s)" num dsc
   (case msz of Nothing -> "variable"; Just x -> show x)

ppEvent :: IntMap EventType -> CapEvent -> String
ppEvent imap (CapEvent cap (Event time spec)) =
  printf "%9d: " time ++
  (case cap of
    Nothing -> ""
    Just c  -> printf "cap %d: " c) ++
  case spec of
    UnknownEvent{ ref=ref } ->
      printf (desc (fromJust (M.lookup (fromIntegral ref) imap)))

    Message msg -> msg
    UserMessage msg -> msg

    other -> showEventTypeSpecificInfo spec

type PutEvents a = PutM a

putE :: Binary a => a -> PutEvents ()
putE = put

runPutEBS :: PutEvents () -> L.ByteString
runPutEBS = runPut

writeEventLogToFile f el = L.writeFile f $ runPutEBS $ putEventLog el

putType :: EventTypeNum -> PutEvents ()
putType = putE

putCap :: Int -> PutEvents ()
putCap c = putE (fromIntegral c :: CapNo)

putMarker :: Word32 -> PutEvents ()
putMarker = putE

putEventLog :: EventLog -> PutEvents ()
putEventLog (EventLog hdr es) = do
    putHeader hdr
    putData es

putHeader :: Header -> PutEvents ()
putHeader (Header ets) = do
    putMarker EVENT_HEADER_BEGIN
    putMarker EVENT_HET_BEGIN
    mapM_ putEventType ets
    putMarker EVENT_HET_END
    putMarker EVENT_HEADER_END
 where
    putEventType (EventType n d msz) = do
        putMarker EVENT_ET_BEGIN
        putType n
        putE $ fromMaybe 0xffff msz
        putE (fromIntegral $ length d :: EventTypeDescLen)
        mapM_ put d
        -- the event type header allows for extra data, which we don't use:
        putE (0 :: Word32)
        putMarker EVENT_ET_END

putData :: Data -> PutEvents ()
putData (Data es) = do
    putMarker EVENT_DATA_BEGIN -- Word32
    mapM_ putEvent es
    putType EVENT_DATA_END -- Word16

eventTypeNum :: EventTypeSpecificInfo -> EventTypeNum
eventTypeNum e = case e of
    CreateThread {} -> EVENT_CREATE_THREAD
    RunThread {} -> EVENT_RUN_THREAD
    StopThread {} -> EVENT_STOP_THREAD
    ThreadRunnable {} -> EVENT_THREAD_RUNNABLE
    MigrateThread {} -> EVENT_MIGRATE_THREAD
    RunSpark {} -> EVENT_RUN_SPARK
    StealSpark {} -> EVENT_STEAL_SPARK
    Shutdown {} -> EVENT_SHUTDOWN
    WakeupThread {} -> EVENT_THREAD_WAKEUP
    StartGC {} -> EVENT_GC_START
    EndGC {} -> EVENT_GC_END
    RequestSeqGC {} -> EVENT_REQUEST_SEQ_GC
    RequestParGC {} -> EVENT_REQUEST_PAR_GC
    CreateSparkThread {} -> EVENT_CREATE_SPARK_THREAD
    Message {} -> EVENT_LOG_MSG
    Startup {} -> EVENT_STARTUP
    EventBlock {} -> EVENT_BLOCK_MARKER
    UserMessage {} -> EVENT_USER_MSG
    GCIdle {} -> EVENT_GC_IDLE
    GCWork {} -> EVENT_GC_WORK
    GCDone {} -> EVENT_GC_DONE
    CapsetCreate {} -> EVENT_CAPSET_CREATE
    CapsetDelete {} -> EVENT_CAPSET_DELETE
    CapsetAssignCap {} -> EVENT_CAPSET_ASSIGN_CAP
    CapsetRemoveCap {} -> EVENT_CAPSET_REMOVE_CAP
    RtsIdentifier {} -> EVENT_RTS_IDENTIFIER
    ProgramArgs {} -> EVENT_PROGRAM_ARGS
    ProgramEnv {} -> EVENT_PROGRAM_ENV
    OsProcessPid {} -> EVENT_OSPROCESS_PID
    OsProcessParentPid{} -> EVENT_OSPROCESS_PPID

putEvent :: Event -> PutEvents ()
putEvent (Event t spec) = do
    putType (eventTypeNum spec)
    put t
    putEventSpec spec

putEventSpec (Startup caps) = do
    putCap (fromIntegral caps)

putEventSpec (EventBlock end cap es) = do
    let block = runPutEBS (mapM_ putEvent es)
    put (fromIntegral (L.length block) + 24 :: Word32)
    putE end
    putE (fromIntegral cap :: CapNo)
    putLazyByteString block

putEventSpec (CreateThread t) = do
    putE t

putEventSpec (RunThread t) = do
    putE t

-- here we assume that ThreadStopStatus fromEnum matches the definitions in
-- EventLogFormat.h
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

putEventSpec (ThreadRunnable t) = do
    putE t

putEventSpec (MigrateThread t c) = do
    putE t
    putCap c

putEventSpec (RunSpark t) = do
    putE t

putEventSpec (StealSpark t c) = do
    putE t
    putCap c

putEventSpec (CreateSparkThread t) = do
    putE t

putEventSpec (WakeupThread t c) = do
    putE t
    putCap c

putEventSpec Shutdown = do
    return ()

putEventSpec RequestSeqGC = do
    return ()

putEventSpec RequestParGC = do
    return ()

putEventSpec StartGC = do
    return ()

putEventSpec GCWork = do
    return ()

putEventSpec GCIdle = do
    return ()

putEventSpec GCDone = do
    return ()

putEventSpec EndGC = do
    return ()

putEventSpec (CapsetCreate cs ct) = do
    putE cs
    putE $ case ct of
            CapsetCustom -> 1 :: Word16
            CapsetOsProcess -> 2
            CapsetClockDomain -> 3
            CapsetUnknown -> 0

putEventSpec (CapsetDelete cs) = do
    putE cs

putEventSpec (CapsetAssignCap cs cp) = do
    putE cs
    putCap cp

putEventSpec (CapsetRemoveCap cs cp) = do
    putE cs
    putCap cp

putEventSpec (RtsIdentifier cs rts) = do
    putE (fromIntegral (length rts) + 4 :: Word16)
    putE cs
    mapM_ putE rts

putEventSpec (ProgramArgs cs as) = do
    let as' = unsep as
    putE (fromIntegral (length as') + 4 :: Word16)
    putE cs
    mapM_ putE as'

putEventSpec (ProgramEnv cs es) = do
    let es' = unsep es
    putE (fromIntegral (length es') + 4 :: Word16)
    putE cs
    mapM_ putE es'

putEventSpec (OsProcessPid cs pid) = do
    putE cs
    putE pid

putEventSpec (OsProcessParentPid cs ppid) = do
    putE cs
    putE ppid

putEventSpec (Message s) = do
    putE (fromIntegral (length s) :: Word16)
    mapM_ putE s

putEventSpec (UserMessage s) = do
    putE (fromIntegral (length s) :: Word16)
    mapM_ putE s

-- [] == []
-- [x] == x\0
-- [x, y, z] == x\0y\0
unsep :: [String] -> String
unsep = concatMap (++"\0") -- not the most efficient, but should be ok
