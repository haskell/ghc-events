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
  pushBytes, readHeader, readEvent,

  -- * EventHandle interface
  -- $eventhandleapi
  EventHandle,
  ehOpen, ehReadEvent,
  -- * For compatibility with old clients
  readEventLogFromFile,
 ) where

import GHC.RTS.Events
import GHC.RTS.EventParserUtils
import GHC.RTS.EventTypes hiding (time, spec)

import Control.Applicative ((<$>), (<*>), Applicative(..))
--import Control.Monad.Reader (runReaderT)
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
  -- | An error in parsing has occurred, returns an error message that the
  -- parser may provide
  | ParseError String

-- $bytestringapi
-- The 'ByteString' based API uses 'EventParserState' to keep track of input
-- that it has received so far. This API should be used when control over
-- input generation is required.

-- | Creates a new, empty 'EventParserState' that is required to run the
-- 'readEvent' function.
newParser :: EventParserState
newParser = ParsingHeader (getToDecoder getHeader)

-- Creates a new parser state for events
-- ByteString is fed to the partial decoder
newParserState :: Maybe Int -> Integer -> Header
               -> Decoder (Maybe Event) -> B.ByteString
               -> EventParserState
newParserState cap remaining header partial bss =
  ParsingEvents ED { edCap = cap
                   , edRemaining = remaining
                   , edHeader = header
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
parseHeader :: Decoder Header -> (ParseResult Event, EventParserState)
parseHeader (Done bs _ header) =
    let emptyDecoder = mkEventDecoder header
        newState = newParserState Nothing 0 header emptyDecoder bs
    in (Incomplete, newState)
parseHeader dec@(Partial {})      = (Incomplete, ParsingHeader dec)
parseHeader dec@(Fail _ _ errMsg) = (ParseError errMsg, ParsingHeader dec)

-- | Returns the 'Header' if it is fully parsed.
readHeader :: EventParserState -> Maybe Header
readHeader (ParsingHeader hd) = Nothing
readHeader (ParsingEvents ed) = Just $ edHeader ed

-- | Parses at most one event from the state (refer to 'ParseResult' datatype) and
-- returns the updated state that can be used to parse the next event.
-- readEvent expects its input to follow the structure of a .eventlog file.
readEvent :: EventParserState -> (ParseResult Event, EventParserState)
readEvent (ParsingHeader hd) = parseHeader hd
readEvent (ParsingEvents ed) = readEvent' ed

readEvent' :: EventDecoder -> (ParseResult Event, EventParserState)
readEvent' (ed@(ED _ remaining header partial)) =
    case partial of
      (Done bs sz (Just event)) ->
        let emptyDecoder = mkEventDecoder header in
        case evSpec event of
          -- TODO the actual EventBlock is skipped. Perhaps it shouldn't be?
          EventBlock _ blockCap newRemaining -> do -- process a new block
            let newState = newParserState (isCap blockCap) newRemaining
                                          header emptyDecoder bs
            readEvent newState
          _ -> do -- other, non-EventBlock event
            let newRemaining = remaining - fromIntegral sz
                newState = newParserState (mkCap ed sz) newRemaining
                                          header emptyDecoder bs
            (Item (Event (evTime event) (evSpec event) (mkCap ed 0)), newState)
      -- Parse returning Nothing means that the event log is complete
      (Done _ _ Nothing) -> (Complete, ParsingEvents ed)
      (Partial _) -> (Incomplete, ParsingEvents ed)
      (Fail _ _ errMsg) -> (ParseError errMsg, ParsingEvents ed)

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

ehReadHeader :: EventHandle -> IO (Maybe Header)
ehReadHeader eh = do
  state <- readIORef $ ehState eh
  case state of
    (ParsingHeader _) -> return Nothing
    (ParsingEvents ed) -> return . Just $ edHeader ed

-- | Reads at most one event from the EventHandle. It is intended called
-- repeadetly, returning one event at a time. Will consume input incrementally in
-- chunks of size that is set when instantiating the 'EventHandle' or less
-- (in cases where the size of available input is smaller than chunk size)
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
-- of malformation/cutoff. NOTE: this function will load the entire file to
-- memory, so it is better to not use it with large event logs. 
{-# DEPRECATED readEventLogFromFile "The incremental parser interface \
should be used" #-}
readEventLogFromFile :: FilePath -> IO (Either String EventLog)
readEventLogFromFile f = do
    bytes <- B.readFile f
    let (events, finalState, status) =
          readEventLogFromFile' (newParser `pushBytes` bytes) []
    let mbHeader = readHeader finalState
    case (mbHeader, status) of
      (_, ParseError errMsg) -> return $ Left $ "Parse error: " ++ errMsg
      (Nothing, _) -> do return $ Left $ \
                         concat $ ["Header was lost during parsing. This "
                                  ,"should never happen. Please report a bug."]
      (Just header, Complete) -> do
        return $ Right $ EventLog header (Data events)
      (Just header, Incomplete) -> do
        hPutStrLn stderr $ concat ["Warning: The event log was not fully ",
                           "parsed. It could have been malformed or incomplete."]
        return $ Right $ EventLog header (Data events)
      _ -> error $ concat ["Error: There was no parse error, Header is intact ",
                            "but the log\ \ is not. This should never happen, ",
                            "please report a bug."]

-- Repeadetly pulls events until EventParserState runs out. Should only be used
-- when all input is fed to the EventParserState already.
readEventLogFromFile' :: EventParserState -> [Event]
                      -> ([Event], EventParserState, ParseResult ())
readEventLogFromFile' eps events =
    case newEvent of
        (Item ev)        -> readEventLogFromFile' newState (ev:events)
        (Complete)       -> (events, newState, Complete)
    -- In incomplete cases we try to call readEvent once more since the first
    -- event may require two readEvent calls to be acquired
        (Incomplete)     -> let (newEvent', newState') = readEvent newState
                            in case newEvent' of
                            (Item e) -> readEventLogFromFile' newState' (e:events)
                            _ -> (events, newState', Incomplete)
        (ParseError err) -> (events, newState, ParseError err)
    where (newEvent, newState) = readEvent eps

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
    event_parsers = standardParsers ++
                    if is_ghc_6
                        then ghc6Parsers
                        else ghc7Parsers
                             ++ mercuryParsers ++ perfParsers
    parsers = EventParsers $ mkEventTypeParsers imap event_parsers

-- Turns an instance of Get into a Decoder
getToDecoder :: Get a -> Decoder a
getToDecoder = runGetIncremental
