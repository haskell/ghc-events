{-|
Module      : GHC.RTS.EventsIncremental
Description : Incremental parser functions for GHC RTS EventLog framewrok
Maintainer  : karolis.velicka@gmail.com

This module contains functions used for parsing *.eventlog files emitted
by the GHC runtime sytem.
-}
{-# LANGUAGE CPP #-}
{-
-}

module GHC.RTS.EventsIncremental (
  ParseResult(..),

  -- * ByteString interface
  -- $bytestringapi
  EventParserState,
  newParserState,
  pushBytes, readHeader, readEvent,

  -- * EventHandle interface
  -- $eventhandleapi
  EventHandle,
  ehOpen, ehReadEvent,
  -- * For compatibility with old clients
  readEventLogFromFile,
  writeEventLogToFile,
  -- * Helper functions
  serialiseEventLog,
  readRemainingEvents,
  printEventsIncremental
 ) where

import GHC.RTS.Events
import GHC.RTS.EventParserUtils
import GHC.RTS.EventTypes hiding (time, spec)

import Data.Binary.Get hiding (remaining)
import Data.Binary.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Control.Concurrent (threadDelay)
import qualified Data.IntMap.Strict as M
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO (Handle, hPutStrLn, stderr)
import Data.Word (Word16, Word32)



#define EVENTLOG_CONSTANTS_ONLY
#include "EventLogFormat.h"

-- | Keeps the currently pushed input and other necessary state for the parsing
data EventParserState = ParsingHeader (Decoder Header)
                      | ParsingEvents EventDecoder

-- | An abstraction over 'Handle' and 'EventParserState' for a simple incemental
-- parsing interface.
data EventHandle =
  EH { ehHandle    :: Handle -- Handle to read from
     , ehChunkSize :: Int -- Chunk size for incremental reading
     , ehState     :: IORef EventParserState -- state for the parser
     }

data EventDecoder =
  ED { -- If in EventBlock, we track it's capability
       edCap       :: Maybe Int
       -- Tracks the number of remaining bytes in an EventBlock
     , edRemaining :: Word32
       -- The full parsed header that is used to create a new decoder once
       -- edPartial returns an 'Item'
     , edHeader   :: Header
       -- A decoder that keeps the currently unconsumed bytestring (and possibly)
       -- the next event to be returned by readEvent
     , edPartial   :: Decoder (Maybe Event) }

-- | Datatype that describes the result of a parse.
data ParseResult a =
  -- | Successfully parsed an item
    Item a
  -- | The log is not finished yet but the input did not contain any more
  -- complete items
  | Incomplete
  -- | Parsing was completed successfully
  | Complete
  -- | An error in parsing has occurred, contains an error message that the
  -- parser may provide
  | ParseError String

-- $bytestringapi
-- The 'ByteString' based API uses 'EventParserState' to hold input
-- that it has received so far. This API takes input in form of 'B.ByteString's
-- so it is up to the user to generate  it.

-- | Creates a new, empty 'EventParserState' that is required to run the
-- 'readEvent' function.
newParserState :: EventParserState
newParserState = ParsingHeader (getToDecoder getHeader)

-- Creates a new parser state for events
-- ByteString is fed to the partial decoder
newParserState' :: Maybe Int -> Word32 -> Header
                -> Decoder (Maybe Event) -> B.ByteString
                -> EventParserState
newParserState' cap remaining header partial bss =
  ParsingEvents ED { edCap = cap
                   , edRemaining = remaining
                   , edHeader = header
                   , edPartial = partial `pushChunk` bss }

-- | Pushes a 'ByteString' to 'EventParserState'. This function is the only
-- supported way of providing input in the ByteString interface.
-- 'pushBytes' expects its input to follow the structure of a .eventlog file.
pushBytes :: EventParserState -> B.ByteString -> EventParserState
pushBytes (ParsingHeader headerDecoder) bs =
    ParsingHeader $ headerDecoder `pushChunk` bs
pushBytes (ParsingEvents ed) bs =
    ParsingEvents $ ed { edPartial = (edPartial ed) `pushChunk` bs}

-- Reads the header and returns it as a result. Is only required for
-- readEventLogFromFile functionality, so may be removed in a future version
parseHeader :: Decoder Header -> (ParseResult Event, EventParserState)
parseHeader (Done bs _ header) =
    let emptyDecoder = mkEventDecoder header
        newState = newParserState' Nothing 0 header emptyDecoder bs
    in (Incomplete, newState)
parseHeader dec@(Partial {})      = (Incomplete, ParsingHeader dec)
parseHeader dec@(Fail _ _ errMsg) = (ParseError errMsg, ParsingHeader dec)

-- | Returns the 'Header' if 'EventParserState' was provided with enough input
-- to parse the it already.
readHeader :: EventParserState -> Maybe Header
readHeader (ParsingHeader _ ) = Nothing
readHeader (ParsingEvents ed) = Just $ edHeader ed

-- | Parses at most one event from the state (cf. 'ParseResult') and
-- returns the updated state that can be used to parse the next event.
readEvent :: EventParserState -> (ParseResult Event, EventParserState)
readEvent (ParsingHeader hd) = parseHeader hd
readEvent (ParsingEvents ed) = readEvent' ed

readEvent' :: EventDecoder -> (ParseResult Event, EventParserState)
readEvent' (ed@(ED _ remaining header partial)) =
    case partial of
      (Done bs sz (Just event)) ->
        let emptyDecoder = mkEventDecoder header in
        case evSpec event of
          EventBlock _ blockCap newRemaining -> do -- process a new block
            let newState = newParserState' (isCap blockCap) newRemaining
                                          header emptyDecoder bs
            readEvent newState
          _ -> do -- other, non-EventBlock event
            let newRemaining = remaining - fromIntegral sz
                newState = newParserState' (mkCap ed sz) newRemaining
                                          header emptyDecoder bs
            (Item (Event (evTime event) (evSpec event) (mkCap ed 0)), newState)
      -- Parse returning Nothing means that the event log is complete
      (Done _ _ Nothing) -> (Complete, ParsingEvents ed)
      (Partial _) -> (Incomplete, ParsingEvents ed)
      (Fail _ _ errMsg) -> (ParseError errMsg, ParsingEvents ed)

-- $eventhandleapi
-- This API uses 'EventHandle' datatype that abstracts away the mutation of
-- state and provides a simple interface for parsing events coming from a file
-- descriptor. Just like the ByteString-based API, the contents of the 'Handle'
-- are expected to follow the order of an .eventlog file.

-- | Instantiates a new EventHandle.
ehOpen :: Handle -- ^ Handle to read the input from. Its contents are expected
                 -- to begin with an .eventlog format header.
       -> Int -- ^ The size of the chunk that the parser will read at once
       -> IO EventHandle
ehOpen handle sz = do
  ioref <- newIORef $ newParserState
  return EH { ehHandle = handle, ehChunkSize = sz, ehState = ioref }

-- | Reads at most one event from the EventHandle. It is intended called
-- repeadetly, returning one event at a time. 
ehReadEvent :: EventHandle -> IO (ParseResult Event)
ehReadEvent (EH handle chunkSize stateRef) = do
  state <- readIORef stateRef
  let (result, state') = readEvent state
  case result of
    (Item _) -> do
      writeIORef stateRef state'
      return result
    (Incomplete) -> do
      bs <- B.hGetSome handle chunkSize
      if bs == B.empty
        then return Incomplete
        else do
          writeIORef stateRef $ state' `pushBytes` bs
          ehReadEvent $ EH handle chunkSize stateRef
    (Complete) -> return Complete
    (ParseError errMsg) -> return (ParseError errMsg)

-- | Reads a full 'EventLog' from file. If the file is incomplete, will still
-- return a properly formed 'EventLog' object with all the events until the point
-- of malformation/cutoff. __NOTE__: in this case user will only be informed via
-- an error message to stderr since this interface does not provide a better
-- alternative.
-- This function will load the entire file to
-- memory, so it is better to not use it with large event logs.
{-# DEPRECATED readEventLogFromFile "The incremental parser interface \
should be used" #-}
readEventLogFromFile :: FilePath -> IO (Either String EventLog)
readEventLogFromFile f = do
    bytes <- B.readFile f
    let (events, finalState, status) =
          readRemainingEvents (newParserState `pushBytes` bytes)
    let mbHeader = readHeader finalState
    case (mbHeader, status) of
      (_, ParseError errMsg) -> return $ Left $ "Parse error: " ++ errMsg
      (Nothing, _) -> do return $ Left $ \
                         concat $ ["Header was lost during parsing. This "
                                  ,"should never happen. Please report a bug."]
      (Just header, Complete) -> do
        -- We reverse the list of events since the original list has them
        -- in reverse order and reversing ensures stability for sorting.
        return $ Right $ EventLog header (Data $ reverse events)
      (Just header, Incomplete) -> do
        hPutStrLn stderr $ concat ["Warning: The event log was not fully ",
                           "parsed. It could have been malformed or incomplete."]
        return $ Right $ EventLog header (Data $ reverse events)
      _ -> error $ concat ["Error: There was no parse error, Header is intact ",
                            "but the log\ \ is not. This should never happen, ",
                            "please report a bug."]

-- | Repeadetly consumes events until 'EventParserState' contains less than a
-- single 'Event'. The last item of output triple indicates whether the full
-- log was parsed or not, or whether there was a parse error,
-- hence the ParseResult of unit datatype.
readRemainingEvents :: EventParserState -> ([Event], EventParserState, ParseResult ())
readRemainingEvents eps = readRemainingEvents' eps []

readRemainingEvents' :: EventParserState -> [Event]
                      -> ([Event], EventParserState, ParseResult ())
readRemainingEvents' eps events =
    case newEvent of
        (Item ev)        -> readRemainingEvents' newState (ev:events)
        (Complete)       -> (events, newState, Complete)
    -- In incomplete cases we try to call readEvent once more since the first
    -- event may require two readEvent calls to be acquired
        (Incomplete)     -> let (newEvent', newState') = readEvent newState
                            in case newEvent' of
                            (Item e) -> readRemainingEvents' newState' (e:events)
                            _ -> (events, newState', Incomplete)
        (ParseError err) -> (events, newState, ParseError err)
    where (newEvent, newState) = readEvent eps

-- | Writes the 'EventLog' to file. The log is expected to __NOT__ have 'EventBlock'
-- markers/events - the parsers no longer emit them and they are handled behind
-- the scenes.
writeEventLogToFile :: FilePath -> EventLog -> IO ()
writeEventLogToFile fp el = do
  BL.writeFile fp $ serialiseEventLog el

-- | Serialises an 'EventLog' back to a 'ByteString', usually for writing it
-- back to a file.
serialiseEventLog :: EventLog -> BL.ByteString
serialiseEventLog el@(EventLog _ (Data events)) =
  runPut $ putEventLog blockedEl
  where
    eventsMap = capSplitEvents events
    blockedEventsMap = M.mapWithKey addBlockMarker eventsMap
    blockedEl = el{dat = Data blockedEvents}
    blockedEvents = M.foldr (++) [] blockedEventsMap

-- Gets the Capability of an event in numeric form
getIntCap :: Event -> Int
getIntCap Event{evCap = cap} =
  case cap of
  Just capNo -> capNo
  Nothing    -> -1

-- Creates an IntMap of the events with capability number as the key.
-- Key -1 indicates global (capless) event
capSplitEvents :: [Event] -> M.IntMap [Event]
capSplitEvents evts = capSplitEvents' evts M.empty

capSplitEvents' :: [Event] -> M.IntMap [Event] -> M.IntMap [Event]
capSplitEvents' evts imap =
  case evts of
  (x:xs) -> capSplitEvents' xs (M.insertWith (++) (getIntCap x) [x] imap)
  []     -> imap

-- Adds a block marker to the beginnng of a list of events, annotated with
-- its capability. All events are expected to belong to the same cap.
addBlockMarker :: Int -> [Event] -> [Event]
addBlockMarker cap evts =
  (Event startTime (EventBlock endTime cap sz) (isCap cap)) : sortedEvts
  where sz = fromIntegral . BL.length $ runPut $ mapM_ putEvent evts
        startTime = case sortedEvts of
          (x:_) -> evTime x
          [] -> error "Cannot add block marker to an empty list of events"
        sortedEvts = sortEvents evts
        endTime = evTime $ last sortedEvts

-- Checks if the capability is not -1 (which indicates a global eventblock), so
-- has no associated capability
isCap :: Int -> Maybe Int
isCap blockCap = if fromIntegral blockCap /= ((-1) :: Word16)
                    then Just blockCap
                    else Nothing

-- Checks if there are any bytes left in the current EventBlock. That number
-- could be negative because exist some blockless events.
mkCap :: EventDecoder -> ByteOffset -> Maybe Int
mkCap ed sz
  | (edRemaining ed - fromIntegral sz) > 0 = edCap ed
  | otherwise = Nothing

-- Makes a decoder with all the required parsers when given a Header
mkEventDecoder :: Header -> Decoder (Maybe Event)
mkEventDecoder header =
    getToDecoder (getEvent parsers)
  where
    imap = M.fromList [ (fromIntegral (num t),t) | t <- eventTypes header]
    -- This test is complete, no-one has extended this event yet and all future
    -- extensions will use newly allocated event IDs.
    is_ghc_6 = Just sz_old_tid == do create_et <- M.lookup EVENT_CREATE_THREAD imap
                                     size create_et
    -- GHC6 writes an invalid header, we handle it here by using a
    -- different set of event parsers.  Note that the ghc7 event parsers
    -- are standard events, and can be used by other runtime systems that
    -- make use of threadscope.

    -- GHC-7.8.2 uses a different thread block status encoding,
    -- and therefore requires a different parser for the stop
    -- event. Later, in GHC-7.8.3, the old encoding was restored.
    -- GHC-7.8.2 can be recognised by presence and absence of
    -- events in the header:
    --   * User markers were added in GHC-7.8 
    --   * an empty event HACK_BUG_T9003 was added in GHC-7.8.3
    -- This fix breaks software which uses ghc-events and combines
    -- user markers with the older stop status encoding. We don't
    -- know of any such software, though.
    is_pre77  = M.notMember EVENT_USER_MARKER imap
    is_ghc782 = M.member EVENT_USER_MARKER imap &&
                M.notMember EVENT_HACK_BUG_T9003 imap

    stopParsers = if is_pre77 then pre77StopParsers
                    else if is_ghc782 then [ghc782StopParser]
                        else [post782StopParser]

    event_parsers = if is_ghc_6
                        then standardParsers ++ ghc6Parsers ++
                            parRTSParsers sz_old_tid
                        else standardParsers ++ ghc7Parsers ++
                            stopParsers ++ parRTSParsers sz_tid ++
                            mercuryParsers ++ perfParsers

    parsers = EventParsers $ mkEventTypeParsers imap event_parsers

-- Turns an instance of Get into a Decoder
getToDecoder :: Get a -> Decoder a
getToDecoder = runGetIncremental

-- | Pretty-prints events coming from a handle
printEventsIncremental :: EventHandle
                       -> Bool -- Whether to retry on incomplete logs
                       -> IO ()
printEventsIncremental eh dashf = do
    event <- ehReadEvent eh
    case event of
      Item ev -> do
          putStrLn (ppEvent' ev) -- if actual printing is needed
          printEventsIncremental eh dashf
      Incomplete ->
        if dashf
          then print "Log Incomplete. Waiting for more input." >> threadDelay 1000000 >> printEventsIncremental eh dashf
          else putStrLn "Finished (NOT all file was parsed successfully)"
      Complete ->
        putStrLn "Finished (file was parsed successfully)"
      ParseError errMsg ->
        putStrLn $ "Error: " ++ errMsg
