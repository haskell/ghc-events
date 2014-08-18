{-# LANGUAGE CPP #-}
{-
 -  Incremental parser functions for GHC RTS EventLog framewrok.
-}

module GHC.RTS.EventsIncremental (
  Result(..),
  EventParserState,

  initEventParser,
  readEvent,
  readEventLogFromFile,
  ppEvent'
 ) where

import GHC.RTS.Events
import GHC.RTS.EventParserUtils
import GHC.RTS.EventTypes

import Control.Concurrent (threadDelay)
import Control.Monad.Reader (runReaderT)
import qualified Data.ByteString as B
import qualified Data.IntMap as M
import Data.Binary.Get
import System.Exit (exitFailure)
import System.IO (IOMode(ReadMode), openBinaryFile, Handle)
import Data.Word (Word16)
import Text.Printf

#define EVENTLOG_CONSTANTS_ONLY
#include "EventLogFormat.h"

type EventParserState = Either (Decoder Header) EventDecoder

data EventDecoder = 
  ED { edCap       :: Maybe Int
     , edRemaining :: Integer
     , edDecoder   :: Decoder (Maybe Event)
     , edPartial   :: Decoder (Maybe Event) }

-- Datatype that describes the result of getEvent
data Result a
  -- Successfully parsed an item
  = One a
  -- The eventlog wasn't complete but the input did not contain any more complete
  -- items
  | PartialEventLog
  -- Parsing was completed successfully
  -- TODO: (what happens if getEvent called again?)
  | CompleteEventLog
  -- An error in parsing has occurred
  | EventLogParsingError String

initEventParser :: EventParserState
initEventParser = Left (getToDecoder getHeader)

-- Creates a new parser state (a Right, so events only). If given a non-empty list
-- of ByteStrings, pushes them all to the partial decoder
-- TODO: Order of lists may be confusing
newParserState :: Maybe Int -> Integer 
               -> Decoder (Maybe Event) -> Decoder (Maybe Event) -> [B.ByteString]
               -> EventParserState
newParserState cap remaining dec partial bss = 
  Right $ ED { edCap = cap
             , edRemaining = remaining
             , edDecoder = dec
             , edPartial = (foldl pushChunk partial bss) }

-- Given a state and a bytestring, parses at most one event (if the BS contains
-- enough data) and keeps the remainder ofthe BS in the state (to be used in 
-- successive call to readEvent). Expects the first bytes to contain a complete Header
readEvent :: EventParserState -> B.ByteString -> (Result Event, EventParserState)
readEvent (Left headerDecoder) bs = 
    case (readHeader headerDecoder bs) of 
      (One _, state) -> readEvent state B.empty
      -- Following cases just to propagate the type
      (EventLogParsingError errMsg, state) -> (EventLogParsingError errMsg, state)
      (PartialEventLog, state) -> (PartialEventLog, state)
      otherwise -> error "The impossible has happened. Report a bug."
readEvent (Right ed) bs = readEvent' ed bs 

readHeader :: Decoder Header -> B.ByteString -> (Result Header, EventParserState)
readHeader dec bs =
    case dec of 
      (Done bs' _ header) ->
        let emptyDecoder = mkEventDecoder header
            newState = newParserState Nothing 0 emptyDecoder emptyDecoder [bs', bs]
        in (One header, newState)
      (Partial {}) ->
        if bs == B.empty
          then (PartialEventLog, Left dec)
          else readHeader (dec `pushChunk` bs) B.empty
      (Fail _ _ errMsg) -> 
        -- TODO: should the state be updated here?
        (EventLogParsingError errMsg, Left dec)  

readEvent' :: EventDecoder -> B.ByteString -> (Result Event, EventParserState)
readEvent' (ed@(ED cap remaining emptyDecoder partial)) bs = 
    case partial of
      (Done bs' sz (Just event)) -> do
        case evSpec event of
          EventBlock _ blockCap newRemaining -> do
            let newState = newParserState (isCap blockCap) newRemaining 
                                          emptyDecoder emptyDecoder [bs', bs]
            readEvent newState B.empty
          otherwise -> do
            let newRemaining = remaining - fromIntegral sz
                newState = newParserState (mkCap ed sz) newRemaining
                                          emptyDecoder emptyDecoder [bs', bs]
            (One (Event (evTime event) (evSpec event) (mkCap ed 0)), newState)
      (Done _ _ Nothing) -> (CompleteEventLog, Right ed)
      (Partial _) -> 
        if bs == B.empty
          then (PartialEventLog, Right ed)
          else let newState = newParserState cap remaining emptyDecoder partial [bs]
               in readEvent newState B.empty
      (Fail _ _ errMsg) -> (EventLogParsingError errMsg, Right ed)

-- TODO: Make Either String EventLog
readEventLogFromFile :: FilePath -> IO EventLog
readEventLogFromFile f = do
    handle  <- openBinaryFile f ReadMode
    (header, state) <- readBareHeader Nothing handle
    events <- readAllEvents (Just state) True handle 
    return $ EventLog header (Data events)

readBareHeader :: Maybe (Decoder Header) -> Handle -> IO (Header, EventParserState)
readBareHeader (Just headerDecoder) handle = do
  !bs <- B.hGetSome handle chunkSize
  case (readHeader headerDecoder bs) of
    (One header, state) -> return (header, state)
    (PartialEventLog, (Left headDec)) -> readBareHeader (Just headDec) handle
    (EventLogParsingError errMsg, state) -> error errMsg
    otherwise -> error "Should never happen."
readBareHeader Nothing handle = readBareHeader (Just (getToDecoder getHeader)) handle

readAllEvents :: Maybe EventParserState -- Nothing to initialise a new parser
              -> Bool -- Retry on incomplete logs? (infinite loop if never completes)
              -> Handle -- Handle to read from
              -> IO [Event]
readAllEvents (Just eps) dashFMode handle = do
    bs <- B.hGetSome handle chunkSize
    let (resEvent, newState) = readEvent eps bs
    case resEvent of
      One ev -> do
          -- TODO: space leak?
          rest <- (readAllEvents (Just newState) dashFMode handle)
          return $ ev : rest 
      PartialEventLog -> do
        if dashFMode
          then do threadDelay 1000000
                  print "waiting for input"
                  readAllEvents (Just newState) dashFMode handle
          else putStrLn "Incomplete but no -f" >> return []
      CompleteEventLog -> return []
      EventLogParsingError errMsg -> 
        putStrLn "A parsing error has occured." >> exitFailure
readAllEvents (Nothing) dashFMode handle = 
  readAllEvents (Just initEventParser) dashFMode handle



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
