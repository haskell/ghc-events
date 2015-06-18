{-# LANGUAGE CPP #-}
{-
 -  Incremental parser functions for GHC RTS EventLog framewrok.
-}

module GHC.RTS.EventsIncremental (
  ParseResult(..),

  -- * ByteString interface
  -- $bytestringapi
  EventParserState,
  newParser,
  pushBytes, readEvent,

  -- * EventHandle interface
  -- $eventhandleapi
  EventHandle,
  ehOpen, ehReadEvent, ehReadEvents,
  -- * For compatibility with old clients
  readEventLogFromFile,
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
import System.IO (IOMode(ReadMode), openBinaryFile, Handle, hPutStrLn, stderr)
import Data.Word (Word16)

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
     , edRemaining :: Integer
       -- An empty decoder that is used for parsing the next event
       -- It decodes a Maybe Event because Nothing indicates the end of an
       -- eventlog in the getEvent parser.
     , edDecoder   :: Decoder (Maybe Event)
       -- A decoder that keeps the currently unconsumed bytestring (and possibly)
       -- the next event to be returned by readEvent
     , edPartial   :: Decoder (Maybe Event) }

-- | Datatype that describes the result of a parse
data ParseResult a =
  -- | Successfully parsed an item
    Item a
  -- | The log is not finished yet but the input did not contain any more
  -- complete items
  | Incomplete
  -- | Parsing was completed successfully
  | Complete
  -- | An error in parsing has occurred, returns a (hopefully useful)
  -- error message.
  | ParseError String

-- $bytestringapi
-- The 'ByteString' based API uses 'EventParserState' to keep track of input
-- that it has received so far. This API should be used when control over
-- input generation is required.

-- | Creates a new, empty 'EventParserState' that can then be passed to the
-- 'readEvent' function.
newParser :: EventParserState
newParser = ParsingHeader (getToDecoder getHeader)

-- Creates a new parser state for events
-- If given a non-empty list of ByteStrings, pushes them all to the partial
-- decoder
newParserState :: Maybe Int -> Integer -> Decoder (Maybe Event)
               -> Decoder (Maybe Event) -> B.ByteString
               -> EventParserState
newParserState cap remaining dec partial bss =
  ParsingEvents ED { edCap = cap
                   , edRemaining = remaining
                   , edDecoder = dec
                   , edPartial = partial `pushChunk` bss }

-- | Pushes a 'ByteString' to 'EventParserState'. This function is the only
-- supported way of providing input in the ByteString interface.
pushBytes :: EventParserState -> B.ByteString -> EventParserState
pushBytes (ParsingHeader headerDecoder) bs =
    ParsingHeader $ headerDecoder `pushChunk` bs
pushBytes (ParsingEvents ed) bs =
    ParsingEvents $ ed { edPartial = (edPartial ed) `pushChunk` bs}

-- Reads the header and returns it as a result. Is only required for
-- readEventLogFromFile functionality, so may be removed in a future version
readHeader :: Decoder Header -> (ParseResult Header, EventParserState)
readHeader (Done bs _ header) =
    let emptyDecoder = mkEventDecoder header
        newState = newParserState Nothing 0 emptyDecoder emptyDecoder bs
    in (Item header, newState)
readHeader dec@(Partial {}) = (Incomplete, ParsingHeader dec)
readHeader dec@(Fail _ _ errMsg) =
    (ParseError errMsg, ParsingHeader dec)

-- Helper to simplify the logic of readEvent
readHeader' :: Decoder Header -> (ParseResult Event, EventParserState)
readHeader' hd =
    case readHeader hd of
      -- Header was parsed completely so we can safely call readEvents, the
      -- state will be ParsingEvents
      (Item _, state) -> readEvent state
      (Incomplete, state) -> (Incomplete, state)
      (ParseError errMsg, state) ->
          (ParseError errMsg, state)
      -- CompleteEventlog can't happen because we don't read the log yet
      _ -> error "The impossible has happened. Report a bug."

-- | Parses at most one event from the state (refer to 'ParseResult' datatype) and
-- returns the updated state that can be used to parse the next event.
-- readEvent expects its input to follow the structure of a .eventlog file.
readEvent :: EventParserState -> (ParseResult Event, EventParserState)
readEvent (ParsingHeader hd) = readHeader' hd
readEvent (ParsingEvents ed) = readEvent' ed

readEvent' :: EventDecoder -> (ParseResult Event, EventParserState)
readEvent' (ed@(ED _ remaining emptyDecoder partial)) =
    case partial of
      (Done bs sz (Just event)) ->
        case evSpec event of
          EventBlock _ blockCap newRemaining -> do
            let newState = newParserState (isCap blockCap) newRemaining
                                          emptyDecoder emptyDecoder bs
            readEvent newState
          _ -> do
            let newRemaining = remaining - fromIntegral sz
                newState = newParserState (mkCap ed sz) newRemaining
                                          emptyDecoder emptyDecoder bs
            (Item (Event (evTime event) (evSpec event) (mkCap ed 0)), newState)
      (Done _ _ Nothing) -> (Complete, ParsingEvents ed)
      (Partial _) -> (Incomplete, ParsingEvents ed)
      (Fail _ _ errMsg) -> (ParseError errMsg, ParsingEvents ed)

{-
-- $eventhandleapi
-- This API uses 'EventHandle' datatype that abstracts away the mutation of
-- state and provides a simple interface for parsing events. Just like
-- the ByteString-based API, events are parsed
-- __in the order that they were written to the .eventlog file.__

-- | Instantiates a new EventHandle.
ehOpen :: Handle -- ^ Handle to read the input from. Its contents are expected
                 -- to begin with an .eventlog format header.
       -> Int -- ^ The number of bytes that the parser will try to read input
              -- from the 'Handle' when needs more input
       -> IO EventHandle
ehOpen handle sz = do
  ioref <- newIORef $ newParser
  return EH { ehHandle = handle, ehChunkSize = sz, ehState = ioref }

-- | Reads at most one event from the EventHandle. Can be called repeadetly. Will
-- consume input incrementally in chunks of size that is set when instantiating
-- the 'EventHandle'.
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

-- Parses a 'Header' to be used for readEventLogFromFile. Should be removed in
-- the near future as that functionality appears to be obsolete.
ehReadHeader :: EventHandle -> IO (ParseResult Header)
ehReadHeader (EH handle chunkSize stateRef) = do
  (ParsingHeader headerDecoder) <- readIORef stateRef
  let (result, state) = readHeader headerDecoder
  case result of
    (Item _) -> do
      writeIORef stateRef state
      return result
    (Incomplete) -> do
      bs <- B.hGetSome handle chunkSize
      if bs == B.empty
        then return $ ParseError "Header is incomplete, terminating"
        else do
          writeIORef stateRef $ state `pushBytes` bs
          ehReadHeader $ EH handle chunkSize stateRef
    (Complete) -> return Complete
    (ParseError errMsg) -> return (ParseError errMsg)

-- TODO: final state that tells how did the parse finish
-- | Reads all available events from an EventHandle. Reports errors to stderr.
ehReadEvents :: EventHandle -> IO [Event]
ehReadEvents = eventRepeater . ehReadEvent
-}

ehOpen = undefined
ehReadEvent = undefined
ehReadEvents = undefined
ehReadHeader = undefined

-- | Reads a full 'EventLog' from file. If the file is incomplete, will still
-- return a properly formed 'EventLog' object with all the events until the point
-- of malformation/cutoff.
readEventLogFromFile :: FilePath -> IO (Either String EventLog)
readEventLogFromFile f = do
    handle <- openBinaryFile f ReadMode
    eh     <- ehOpen handle 1048576
    resultHeader <- ehReadHeader eh
    case resultHeader of
      (ParseError errMsg) -> error errMsg
      (Item header) -> do
        !events <- ehReadEvents eh
        return $ Right $ EventLog header (Data events)
      _ -> error "Should never happen"


eventRepeater :: IO (ParseResult Event) -> IO [Event]
eventRepeater eventReader = do
  event <- eventReader
  case event of
    (Item a) -> (:) <$> return a <*> eventRepeater eventReader
    (Incomplete) -> do
      hPutStrLn stderr "Handle did not contain a complete EventLog."
      return []
    (Complete) -> return []
    (ParseError errMsg) -> hPutStrLn stderr errMsg >> return []

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
