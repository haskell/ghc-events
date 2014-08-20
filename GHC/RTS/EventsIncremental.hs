{-# LANGUAGE CPP #-}
{-
 -  Incremental parser functions for GHC RTS EventLog framewrok.
-}

module GHC.RTS.EventsIncremental (
  Result(..),

  -- * ByteString interface
  -- $bytestringapi
  EventParserState,
  initEventParser,
  pushBytes, readEvent,

  -- * EventHandle interface
  -- $eventhandleapi
  EventHandle,
  ehOpen, ehReadEvent, ehReadEvents,
  -- * For compatibility with old clients
  readEventLogFromFile,
  ppEvent'
 ) where

import GHC.RTS.Events
import GHC.RTS.EventParserUtils
import GHC.RTS.EventTypes hiding (time, spec)

import Control.Applicative ((<$>), (<*>), Applicative(..))
import Control.Monad.Reader (runReaderT)
import Data.Binary.Get hiding (remaining)
import qualified Data.ByteString as B
import qualified Data.IntMap as M
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO (IOMode(ReadMode), openBinaryFile, Handle)
import Data.Word (Word16)
import Text.Printf

#define EVENTLOG_CONSTANTS_ONLY
#include "EventLogFormat.h"

-- | Left is used for keeping the state while parsing a header, Right keeps the
-- state of Event parser
type EventParserState = Either (Decoder Header) EventDecoder

-- | An abstraction over a 'Handle' and state for a simple incemental parsing
-- interface.
data EventHandle =
  EH { ehHandle    :: Handle -- Handle to read from
     , ehChunkSize :: Int -- Chunk size for incremental reading
     , ehState     :: IORef EventParserState -- state for the parser
     }

data EventDecoder = 
  ED { -- If in EventBlock, we track it's capability
       edCap       :: Maybe Int 
       -- Tracks the number of remaining bytes in an EventBlock
     , edRemaining :: Integer 
       -- An empty decoder that is used for parsing the next event
     , edDecoder   :: Decoder (Maybe Event)
       -- A decoder that keeps the currently unconsumed bytestring in itself
     , edPartial   :: Decoder (Maybe Event) }

-- | Datatype that describes the result of a parse
data Result a = 
  -- | Successfully parsed an item
    One a
  -- | The eventlog wasn't complete but the input did not contain any more 
  -- complete items
  | PartialEventLog
  -- | Parsing was completed successfully
  | CompleteEventLog
  -- | An error in parsing has occurred, returns a (hopefully useful)
  -- error message.
  | EventLogParsingError String

-- $bytestringapi
-- The 'ByteString' based API uses 'EventParserState' to keep track of input
-- that it has received so far. This API should be used when control over 
-- input generation is required.

-- | Creates a new 'EventParserState' that can then be passed to the 
-- 'readEvent' function.
initEventParser :: EventParserState
initEventParser = Left (getToDecoder getHeader)

-- Creates a new parser state (a Right, so events only). 
-- If given a non-empty list of ByteStrings, pushes them all to the partial
-- decoder
-- TODO: Order of lists may be confusing
newParserState :: Maybe Int -> Integer -> Decoder (Maybe Event)
               -> Decoder (Maybe Event) -> [B.ByteString]
               -> EventParserState
newParserState cap remaining dec partial bss = 
  Right ED { edCap = cap
           , edRemaining = remaining
           , edDecoder = dec
           , edPartial = foldl pushChunk partial bss }

-- | Pushes a 'ByteString' to 'EventParserState'. This function is the only
-- supported way of providing input in the ByteString interface.
pushBytes :: EventParserState -> B.ByteString -> EventParserState
pushBytes (Left headerDecoder) bs = Left $ headerDecoder `pushChunk` bs
pushBytes (Right ed) bs =
    Right $ ed { edPartial = (edPartial ed) `pushChunk` bs}

readHeader :: Decoder Header -> (Result Header, EventParserState)
readHeader dec =
    case dec of 
      (Done bs _ header) ->
        let emptyDecoder = mkEventDecoder header
            newState = newParserState Nothing 0 emptyDecoder emptyDecoder [bs]
        in (One header, newState)
      (Partial {}) -> (PartialEventLog, Left dec)
      (Fail _ _ errMsg) -> (EventLogParsingError errMsg, Left dec)  
        
-- | Parses at most one event from the state (refer to 'Result' datatype) and
-- returns the updated state that can be used to parse the next event.
-- readEvent expects its input to follow the structure of a .eventlog file.
readEvent :: EventParserState -> (Result Event, EventParserState)
readEvent (Left headerDecoder) = 
    case readHeader headerDecoder of 
      (One _, state) -> readEvent state 
      (PartialEventLog, state) ->
          (PartialEventLog, state)
      (EventLogParsingError errMsg, state) ->
          (EventLogParsingError errMsg, state)
      _ -> error "The impossible has happened. Report a bug."
readEvent (Right ed) = readEvent' ed 

readEvent' :: EventDecoder -> (Result Event, EventParserState)
readEvent' (ed@(ED _ remaining emptyDecoder partial)) = 
    case partial of
      (Done bs sz (Just event)) ->
        case evSpec event of
          EventBlock _ blockCap newRemaining -> do
            let newState = newParserState (isCap blockCap) newRemaining 
                                          emptyDecoder emptyDecoder [bs]
            readEvent newState 
          _ -> do
            let newRemaining = remaining - fromIntegral sz
                newState = newParserState (mkCap ed sz) newRemaining
                                          emptyDecoder emptyDecoder [bs]
            (One (Event (evTime event) (evSpec event) (mkCap ed 0)), newState)
      (Done _ _ Nothing) -> (CompleteEventLog, Right ed)
      (Partial _) -> (PartialEventLog, Right ed)
      (Fail _ _ errMsg) -> (EventLogParsingError errMsg, Right ed)

-- $eventhandleapi
-- This API uses 'EventHandle' datatype that abstracts away the mutation of
-- state and provides a simple interface for parsing events. Just like 
-- the ByteString-based API, events are parsed 
-- __in the order that they were written to the .eventlog file.__

-- | Creates a new event handle. The input handle is epxected to begin at the
-- beginning of file.
ehOpen :: Handle -- ^ Handle to read the input from 
       -> Int -- ^ The number of bytes that the parser will try to read input 
              -- from the 'Handle' when needs more input 
       -> IO EventHandle
ehOpen handle sz = do
  ioref <- newIORef $ initEventParser
  return EH { ehHandle = handle, ehChunkSize = sz, ehState = ioref }

-- Reads at most one event from the EventHandle. Can be called repeadetly
ehReadEvent :: EventHandle -> IO (Result Event)
ehReadEvent (EH handle chunkSize stateRef) = do
  state <- readIORef stateRef
  let (result, state') = readEvent state
  case result of
    (One _) -> do
      writeIORef stateRef state'
      return result
    (PartialEventLog) -> do
      bs <- B.hGetSome handle chunkSize
      if bs == B.empty
        then return PartialEventLog
        else do
          writeIORef stateRef $ state' `pushBytes` bs
          ehReadEvent $ EH handle chunkSize stateRef
    (CompleteEventLog) -> return CompleteEventLog
    (EventLogParsingError errMsg) -> return (EventLogParsingError errMsg)

ehReadHeader :: EventHandle -> IO (Result Header)
ehReadHeader (EH handle chunkSize stateRef) = do
  (Left headerDecoder) <- readIORef stateRef
  let (result, state) = readHeader headerDecoder
  case result of
    (One _) -> do
      writeIORef stateRef state
      return result
    (PartialEventLog) -> do
      bs <- B.hGetSome handle chunkSize
      if bs == B.empty
        then return $ EventLogParsingError "Header is incomplete, terminating"
        else do
          writeIORef stateRef $ state `pushBytes` bs
          ehReadHeader $ EH handle chunkSize stateRef
    (CompleteEventLog) -> return CompleteEventLog
    (EventLogParsingError errMsg) -> return (EventLogParsingError errMsg)

readEventLogFromFile :: FilePath -> IO (Either String EventLog)
readEventLogFromFile f = do
    handle <- openBinaryFile f ReadMode
    eh     <- ehOpen handle 1048576
    resultHeader <- ehReadHeader eh
    case resultHeader of
      (EventLogParsingError errMsg) -> error errMsg
      (One header) -> do
        !events <- ehReadEvents eh
        return $ Right $ EventLog header (Data events)
      _ -> error "Should never happen"

ehReadEvents :: EventHandle -> IO [Event]
ehReadEvents = eventRepeater . ehReadEvent

eventRepeater :: IO (Result Event) -> IO [Event]
eventRepeater eventReader = do
  event <- eventReader
  case event of
    (One a) -> (:) <$> return a <*> eventRepeater eventReader
    (EventLogParsingError _) -> return []
    _ -> return []

isCap :: Int -> Maybe Int
isCap blockCap = if fromIntegral blockCap /= ((-1) :: Word16)
                    then Just blockCap
                    else Nothing

mkCap :: EventDecoder -> ByteOffset -> Maybe Int
mkCap ed sz
  | (edRemaining ed - fromIntegral sz) > 0 = edCap ed
  -- TODO: Needs a warning less than 0, it shouldn't happen
  | otherwise = Nothing

-- Makes a decoder with all the required parsers when given a Header
mkEventDecoder :: Header -> Decoder (Maybe Event)
mkEventDecoder header = 
    getToDecoder (runReaderT (getEvent parsers) parsers)
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
    event_parsers = standardParsers ++
                    if is_ghc_6
                        then ghc6Parsers
                        else ghc7Parsers
                             ++ mercuryParsers ++ perfParsers
    parsers = EventParsers $ mkEventTypeParsers imap event_parsers

-- Turns an instance of Get into a Decoder
getToDecoder :: Get a -> Decoder a
getToDecoder = runGetIncremental

ppEvent' :: Event -> String
ppEvent' (Event time spec evCap) =
  printf "%9d: " time ++
  (case evCap of
    Nothing -> ""
    Just c  -> printf "cap %d: " c) ++
  case spec of
    UnknownEvent{ ref=ref } ->
      printf "Unknown Event (ref: %d)" ref
    _ -> showEventInfo spec
