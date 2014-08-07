{-# LANGUAGE CPP,BangPatterns,PatternGuards #-}
{-# OPTIONS_GHC -funbox-strict-fields -fwarn-incomplete-patterns #-}
{- 
 -  Incremental parser functions for GHC RTS EventLog framewrok.
 -}

 module GHC.RTS.EventsIncremental (
  EventHandle,
  openEventHandle,
  readEvent,
  ppEvent'
 ) where

import GHC.RTS.Events
import GHC.RTS.EventParserUtils
import GHC.RTS.EventTypes

import Control.Monad.Reader
import qualified Data.ByteString as B
import qualified Data.IntMap as M
import Data.Binary.Get 
import Data.IORef (IORef, readIORef, writeIORef, newIORef)
import Data.Word (Word16)
import Debug.Trace (traceShow)
import System.IO (Handle, hIsEOF, IOMode(..), withFile, hTell)
import System.Exit (exitFailure)
import Text.Printf

#define EVENTLOG_CONSTANTS_ONLY
#include "EventLogFormat.h"

-- Datatype that holds the reference to an eventlog
data EventHandle
  = EH { eh_hdl     :: Handle
       , eh_decoder :: Decoder (Maybe Event) -- the default decoder 
       , eh_state   :: IORef EventHandleState 
       }

data EventHandleState
  = EHS { ehs_cap       :: Maybe Int
        , ehs_remaining :: Integer -- Bytes remaining in a EventBlock        
        , ehs_decoder   :: Decoder (Maybe Event)
        }

chunkSize :: Int
chunkSize = 1024

updateEHS :: IORef EventHandleState -> Maybe Int -> Integer -> 
             Decoder (Maybe Event) -> IO ()
updateEHS ref cap remaining dec =
  writeIORef ref EHS { ehs_cap = cap, ehs_remaining = remaining, ehs_decoder = dec }

-- Turns an instance of Get into a Decoder
getToDecoder :: Get a -> Decoder a
getToDecoder = runGetIncremental

-- Given a Decoder, repeadetly reads input from Handle h in chunks of size
-- sz bytes until the output is produced or fails.
parseIncremental :: Decoder a -> Handle -> Int -> IO (Decoder a)
parseIncremental dec@(Fail _ _ _) hdl sz = return dec
parseIncremental dec@(Done _ _ _) hdl sz = return dec
parseIncremental dec@(Partial _)  hdl sz = do
    ch <- B.hGetSome hdl sz
    if ch == B.empty      
      then return dec
      else parseIncremental (dec `pushChunk` ch) hdl sz

-- Uses the incremental parser to parse an item completely, returning the item and 
-- unconsumed bytestring
-- TODO: Clarify behaviour with Simon eof check doesn't work for file-based
parseStrict :: Decoder a -> Handle -> Int -> B.ByteString -> IO (a, B.ByteString)
parseStrict dec@(Fail _ _ errMsg) hdl sz bs = putStrLn errMsg >> exitFailure
parseStrict dec@(Done bs' _ dat)  hdl sz bs = return (dat, bs')
parseStrict dec@(Partial _)       hdl sz bs = do
    ch  <- if bs == B.empty
              then B.hGetSome hdl sz
              else return bs
    eof <- hIsEOF hdl
    if eof     
      then parseStrict (pushEndOfInput dec) hdl sz bs
      else parseStrict (dec `pushChunk` ch) hdl sz bs

-- Given a Handle, initialises a corresponding EventHandle
openEventHandle :: Handle -> IO EventHandle
openEventHandle handle = do
    -- Reading in 1MB chunks, not sure whether it's a resonable default
    -- Discard offset because it doesn't matter for headers
    (!header, !bs') <- parseStrict (getToDecoder getHeader) handle chunkSize B.empty
    let imap = M.fromList [ (fromIntegral (num t),t) | t <- eventTypes header]
        -- This test is complete, no-one has extended this event yet and all future
        -- extensions will use newly allocated event IDs.
        is_ghc_6 = Just sz_old_tid == do create_et <- M.lookup EVENT_CREATE_THREAD imap
                                         size create_et
        -- GHC6 writes an invalid header, we handle it here by using a
        -- different set of event parsers.  Note that the ghc7 event parsers
        -- are standard events, and can be used by other runtime systems that
        -- make use of threadscope.
        event_parsers = if is_ghc_6
                            then standardParsers ++ ghc6Parsers
                            else standardParsers ++ ghc7Parsers
                                 ++ mercuryParsers ++ perfParsers
        parsers = EventParsers $ mkEventTypeParsers imap event_parsers
        dec = getToDecoder (runReaderT (getEvent parsers) parsers)
    ioref <- newIORef $ EHS { ehs_cap=Nothing
                            , ehs_remaining = 0
                            ,ehs_decoder = dec `pushChunk` bs' }
    return $ EH handle dec ioref


-- Gets a single Event, nothing if no more events are available (either EOF
-- or log is incomplete)
readEvent :: EventHandle -> IO (Result CapEvent)
readEvent (eh@EH { eh_decoder = eh_dec, eh_hdl = hdl, eh_state = ref})
  = do 
    EHS { ehs_cap = cap, ehs_remaining = remaining, ehs_decoder = dec } <- readIORef ref
    if remaining > 0
      then do -- we are in the middle of an EventBlock
        dec' <- parseIncremental dec hdl chunkSize
        case dec' of 
          (Done bs sz (Just e)) -> do
            updateEHS ref cap (remaining - (fromIntegral sz)) (eh_dec `pushChunk` bs)
            return $ One (CapEvent { ce_cap = cap, ce_event = e })
          (Done bs sz Nothing) -> return $ CompleteEventLog
          (Partial k) -> do
            updateEHS ref cap remaining dec'
            return PartialEventLog
          (Fail _ _ errMsg) -> do
            return $ EventLogParsingError errMsg
      else do -- we are out of an EventBlock
        dec' <- parseIncremental dec hdl chunkSize
        case dec' of 
          (Done bs sz (Just ev)) -> do
            case spec ev of
              EventBlock _ block_cap new_sz -> do
                let new_cap = if (fromIntegral block_cap /= ((-1) :: Word16))
                              then Just block_cap
                              else Nothing
                updateEHS ref new_cap new_sz (eh_dec `pushChunk` bs)
                readEvent eh
              otherwise -> return $ One (CapEvent { ce_cap = Nothing, ce_event = ev })
          (Done bs sz Nothing) -> return $ CompleteEventLog
          (Partial k) -> do
            updateEHS ref Nothing 0 dec'
            return PartialEventLog
          (Fail _ _ errMsg) -> do
            return $ EventLogParsingError errMsg

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