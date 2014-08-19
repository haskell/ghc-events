{-# LANGUAGE CPP #-}
{-
 -  Incremental parser functions for GHC RTS EventLog framewrok.
-}

module GHC.RTS.EventsIncremental (
  Result(..),
  EventParserState,
  EventHandle,

  initEventParser,
  pushBytes, readEvent,
  ehOpen, ehReadEvent,
  -- For compatibility with old clients
  readEventLogFromFile,
  ppEvent'
 ) where

import GHC.RTS.Events
import GHC.RTS.EventParserUtils
import GHC.RTS.EventTypes

import Control.Applicative ((<$>), (<*>), Applicative(..))
import Control.Concurrent (threadDelay)
import Control.Monad.Reader (runReaderT)
import Data.Binary.Get
import qualified Data.ByteString as B
import Data.Either (lefts)
import qualified Data.IntMap as M
import Data.IORef (IORef(..), newIORef, readIORef)
import System.Exit (exitFailure)
import System.IO (IOMode(ReadMode), openBinaryFile, Handle)
import Data.Word (Word16)
import Text.Printf

#define EVENTLOG_CONSTANTS_ONLY
#include "EventLogFormat.h"

type EventParserState = Either (Decoder Header) EventDecoder

data EventHandle =
  EH { ehHandle :: Handle
     , ehState  :: IORef EventParserState }

data EventDecoder = 
  ED { edCap       :: Maybe Int
     , edRemaining :: Integer
     , edDecoder   :: Decoder (Maybe Event)
     , edPartial   :: Decoder (Maybe Event) }

-- Datatype that describes the result of getEvent
data Result a
  -- Successfully parsed an item
  = One a
  -- The eventlog wasn't complete but the input did not contain any more 
  -- complete items
  | PartialEventLog
  -- Parsing was completed successfully
  -- TODO: (what happens if getEvent called again?)
  | CompleteEventLog
  -- An error in parsing has occurred
  | EventLogParsingError String

initEventParser :: EventParserState
initEventParser = Left (getToDecoder getHeader)

-- Creates a new parser state (a Right, so events only). 
-- If given a non-empty list
-- of ByteStrings, pushes them all to the partial decoder
-- TODO: Order of lists may be confusing
newParserState :: Maybe Int -> Integer -> Decoder (Maybe Event)
               -> Decoder (Maybe Event) -> [B.ByteString]
               -> EventParserState
newParserState cap remaining dec partial bss = 
  Right $ ED { edCap = cap
             , edRemaining = remaining
             , edDecoder = dec
             , edPartial = (foldl pushChunk partial bss) }

-- Pushes the given bytestring into EventParserState
pushBytes :: EventParserState -> B.ByteString -> EventParserState
pushBytes (Left headerDecoder) bs = Left $ headerDecoder `pushChunk` bs
pushBytes (Right ed) bs = newParserState (edCap ed) (edRemaining ed) 
                                  (edDecoder ed) (edPartial ed) [bs]

readHeader :: Decoder Header -> (Result Header, EventParserState)
readHeader dec =
    case dec of 
      (Done bs _ header) ->
        let emptyDecoder = mkEventDecoder header
            newState = newParserState Nothing 0 emptyDecoder emptyDecoder [bs]
        in (One header, newState)
      (Partial {}) -> (PartialEventLog, Left dec)
      (Fail _ _ errMsg) -> (EventLogParsingError errMsg, Left dec)  
        
-- Parses at most one event from the state (refer to Result datatype)
-- Also returns the updated state
-- Expects the first bytes to contain a complete eventlog header
readEvent :: EventParserState -> (Result Event, EventParserState)
readEvent (Left headerDecoder) = 
    case (readHeader headerDecoder) of 
      (One _, state) -> readEvent state 
      (PartialEventLog, state) ->
          (PartialEventLog, state)
      (EventLogParsingError errMsg, state) ->
          (EventLogParsingError errMsg, state)
      otherwise -> error "The impossible has happened. Report a bug."
readEvent (Right ed) = readEvent' ed 

readEvent' :: EventDecoder -> (Result Event, EventParserState)
readEvent' (ed@(ED cap remaining emptyDecoder partial)) = 
    case partial of
      (Done bs sz (Just event)) -> do
        case evSpec event of
          EventBlock _ blockCap newRemaining -> do
            let newState = newParserState (isCap blockCap) newRemaining 
                                          emptyDecoder emptyDecoder [bs]
            readEvent newState 
          otherwise -> do
            let newRemaining = remaining - fromIntegral sz
                newState = newParserState (mkCap ed sz) newRemaining
                                          emptyDecoder emptyDecoder [bs]
            (One (Event (evTime event) (evSpec event) (mkCap ed 0)), newState)
      (Done _ _ Nothing) -> (CompleteEventLog, Right ed)
      (Partial _) -> (PartialEventLog, Right ed)
      (Fail _ _ errMsg) -> (EventLogParsingError errMsg, Right ed)

-- EventHandle based API

-- Creates a new event handle. The input handle is epxected to begin at the
-- beginning of file.
ehOpen :: Handle -> IO EventHandle
ehOpen handle = do
  ioref <- newIORef $ Left (getToDecoder getHeader)
  return $ EH handle ioref

-- Reads at most one event from the EventHandle. Can be called repeadetly
ehReadEvent :: EventHandle -> IO (Result Event)
ehReadEvent (EH handle stateRef) = do
  state <- readIORef stateRef
  let (result, state') = readEvent state
  case result of
    (One ev) -> return result
    (PartialEventLog) -> do
      bs <- B.hGetSome handle chunkSize
      if bs == B.empty
        then return PartialEventLog
        else do
          let newState = state' `pushBytes` bs
          newRef <- newIORef newState
          ehReadEvent $ EH handle newRef
    (CompleteEventLog) -> return CompleteEventLog
    (EventLogParsingError errMsg) -> return (EventLogParsingError errMsg)

ehReadHeader :: EventHandle -> IO (Result Header)
ehReadHeader (EH handle stateRef) = do
  (Left headerDecoder) <- readIORef stateRef
  let (result, state) = readHeader headerDecoder
  case result of
    (One _) -> return result
    (PartialEventLog) -> do
      bs <- B.hGetSome handle chunkSize
      if bs == B.empty
        then return $ EventLogParsingError "Header is incomplete, terminating"
        else do
          let newState = state `pushBytes` bs
          newRef <- newIORef newState
          ehReadHeader $ EH handle newRef
    (CompleteEventLog) -> return CompleteEventLog
    (EventLogParsingError errMsg) -> return (EventLogParsingError errMsg)

-- TODO: Make Either String EventLog
readEventLogFromFile :: FilePath -> IO (Either String EventLog)
readEventLogFromFile f = do
    handle <- openBinaryFile f ReadMode
    eh     <- ehOpen handle
    resultHeader <- ehReadHeader eh
    case resultHeader of
      (EventLogParsingError errMsg) -> error errMsg
      (One header) -> do
        events <- ehReadEvents $ ehReadEvent eh
        return $ Right $ EventLog header (Data events)
      otherwise -> error "Should never happen"

ehReadEvents :: IO (Result Event) -> IO [Event]
ehReadEvents eventReader = do
  event <- eventReader
  case event of
    (One a) -> (:) <$> return a <*> ehReadEvents eventReader
    otherwise -> return []

-- Parser will read from a Handle in chunks of chunkSize bytes
chunkSize :: Int
chunkSize = 1024

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
      printf "(desc (fromJust (M.lookup (fromIntegral ref) imap)))"
    other -> showEventInfo spec
