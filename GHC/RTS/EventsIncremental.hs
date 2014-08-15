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

import Control.Monad.Reader (runReaderT)
import qualified Data.ByteString as B
import qualified Data.IntMap as M
import Data.Binary.Get
import Data.Word (Word16)
import Text.Printf

#define EVENTLOG_CONSTANTS_ONLY
#include "EventLogFormat.h"

type EventDecoders = (Decoder (Maybe Event), Decoder (Maybe Event))

data EventParserState
  = EPS { epsCap :: Maybe Int -- The capability for the current event
        , epsRemaining :: Integer -- Bytes remaining in an EventBlock
        -- Either the header parser if the header is not completely parsed
        -- yet or a pair of decoders (empty, partial) for parsing events
        , epsDecoder :: Either (Decoder Header) EventDecoders}

-- Datatype that describes the result of getEvent
data Result a
  -- Successfully parsed one item
  = One a
  -- The eventlog wasn't complete but there wasn't any more input in the handle
  | PartialEventLog
  -- Parsing was completed successfully
  -- TODO: (what happens if getEvent called again?)
  | CompleteEventLog
  -- An error in parsing has occurred
  | EventLogParsingError String

newParserState :: Maybe Int 
                  -> Integer 
                  -> Either (Decoder Header) EventDecoders 
                  -> EventParserState
newParserState cap remaining eitherDec = 
  EPS { epsCap = cap
      , epsRemaining = remaining
      , epsDecoder = eitherDec}

initEventParser :: EventParserState
initEventParser = EPS { epsCap = Nothing
                      , epsRemaining = 0
                      , epsDecoder = Left (getToDecoder getHeader)}

readHeader :: Decoder Header -> B.ByteString -> (Result CapEvent, EventParserState)
readHeader dec bs = do
    let (hdr, state) = readHeader' dec bs
    case hdr of 
      (One header) -> do
        let eventDecoder = mkEventDecoder header
            -- push remainder and the new bytestring into the partial decoder
            partial = eventDecoder `pushChunk` bs
            newState = newParserState Nothing 0 (Right (eventDecoder, partial))
        readEvent newState B.empty
      (PartialEventLog) -> (PartialEventLog, state)
      (Fail _ _ errMsg) -> (EventLogParsingError errMsg, initEventParser)  
      (CompleteEventLog) -> error "Should never happen."

readHeader' :: Decoder Header -> B.ByteString -> (Result Header, EventParserState)
readHeader' dec bs =
    case dec of 
      (Done bs' _ header) -> do
        let eventDecoder = mkEventDecoder header
            -- push remainder and the new bytestring into the partial decoder
            partial = eventDecoder `pushChunk` bs' `pushChunk` bs
            newState = newParserState Nothing 0 (Right (eventDecoder, partial))
            return (One header, newState)
      (part@Partial {}) -> do
        if bs == B.empty
          then (PartialEventLog, (newParserState Nothing 0 (Left part)))
          else let newState = newParserState Nothing 0 (Left (part `pushChunk` bs))
               in readHeader' newState B.empty
      (Fail _ _ errMsg) -> 
        -- TODO: should the state be updated here?
        (EventLogParsingError errMsg, initEventParser)  

-- Returns one event if there is enough data passed to it. Reads the header first if
-- necessary 
readEvent :: EventParserState -> B.ByteString -> (Result CapEvent, EventParserState)
readEvent (eps@EPS {epsCap = _, epsRemaining = _, epsDecoder = dec}) bs = 
    case dec of 
      (Left  headerDecoder) -> readHeader headerDecoder bs
      (Right eventDecoders) -> readEvent' eps eventDecoders bs 

readEvent' :: EventParserState -> EventDecoders -> 
              B.ByteString -> (Result CapEvent, EventParserState)
readEvent' (eps@EPS{epsCap = cap, epsRemaining = remaining, epsDecoder = _}) 
           (newDecoder, partial) bs =
    if remaining > 0
      then -- we are in the middle of an EventBlock
        case partial of
          (Done bs' sz (Just e)) -> do
            -- TODO: Make a helper function
            let newPartial = newDecoder `pushChunk` bs' `pushChunk` bs
                newState = newParserState cap (remaining - fromIntegral sz) 
                       (Right (newDecoder, newPartial))
            (One CapEvent { ce_cap = cap, ce_event = e }, newState)
          (Done _ _ Nothing) -> (CompleteEventLog, eps)
          (Partial _) -> 
            if bs == B.empty
              then (PartialEventLog, eps)
              else let newPartial = partial `pushChunk` bs
                       newState = newParserState cap remaining 
                                                 (Right (newDecoder, newPartial))
                   in
                   readEvent newState B.empty
          (Fail _ _ errMsg) -> (EventLogParsingError errMsg, eps)
      else -- we are out of an EventBlock
        case partial of
          (Done bs' sz (Just ev)) ->
            case spec ev of
              EventBlock _ blockCap newRemaining -> do
                let newPartial = newDecoder `pushChunk` bs' `pushChunk` bs
                    newState = newParserState (isCap blockCap) newRemaining 
                                              (Right (newDecoder, newPartial))
                readEvent newState B.empty
              otherwise -> do
                let newPartial = newDecoder `pushChunk` bs' `pushChunk` bs
                    newState = newParserState Nothing 0
                                              (Right (newDecoder, newPartial))
                (One CapEvent { ce_cap = Nothing, ce_event = ev }, newState)
          (Done _ _ Nothing) -> (CompleteEventLog, eps)
          (Partial _) -> 
            if bs == B.empty
              then (PartialEventLog, eps)
              else let newPartial = partial `pushChunk` bs
                       newState = newParserState cap remaining 
                                                 (Right (newDecoder, newPartial))
                   in
                   readEvent newState B.empty
          (Fail _ _ errMsg) -> (EventLogParsingError errMsg, eps)


readEventLogFromFile :: FilePath -> IO (EventLog)
readEventLogFromFile f = do
    h <- hOpen f ReadMode
    header <- readBareHeader h
    events <- readAllEvents True h >>= sortEvents
    return $ EventLog header (Data events)


readAllEvents :: Maybe EventParserState -- Nothing to initialise a new parser
              -> Bool -- Retry on incomplete logs? (infinite loop if never completes)
              -> Handle -- Handle to read from
              -> IO [CapEvent]
readAllEvents (Just eps) dashFMode handle = do
    bs <- B.hGetSome handle chunkSize
    let (resEvent, newState) = readEvent eps bs
    case resEvent of
      One ev -> return $ ev : (readAllEvents (Just newState) dashFMode handle)
      PartialEventLog -> do
        if dashFMode
          then do threadDelay 1000000
                  readAllEvents (Just newState) dashFMode handle
          else putStrLn "Incomplete but no -f" >> return []
      CompleteEventLog -> return []
      EventLogParsingError errMsg -> 
        putStrLn "A parsing error has occured." >> exitFailure
readAllEvents (Nothing) dashFMode handle = do
    readAllEvents (Just initEventParser) dashFMode handle



-- Parser will read from a Handle in chunks of chunkSize bytes
chunkSize :: Int
chunkSize = 1024

isCap :: Int -> Maybe Int
isCap blockCap = if fromIntegral blockCap /= ((-1) :: Word16)
                    then Just blockCap
                    else Nothing

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

ppEvent' :: CapEvent -> String
ppEvent' (CapEvent cap (Event time spec)) =
  printf "%9d: " time ++
  (case cap of
    Nothing -> ""
    Just c  -> printf "cap %d: " c) ++
  case spec of
    UnknownEvent{ ref=ref } ->
      printf "(desc (fromJust (M.lookup (fromIntegral ref) imap)))"

    other -> showEventInfo spec
