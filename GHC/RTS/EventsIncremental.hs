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
  = EH { eh_hdl :: Handle
       , eh_decoder :: Decoder (Result Event)
       , eh_state :: IORef EventHandleState }

data EventHandleState
  = EHS { ehs_cap       :: Maybe Int
        , ehs_remaining :: Integer  -- Bytes remaining in a EventBlock
        , ehs_bs        :: B.ByteString -- Un-read part of input
        } deriving Show

chunkSize :: Int
chunkSize = 1024

updateEHS :: IORef EventHandleState -> Maybe Int -> Integer -> B.ByteString -> IO ()
updateEHS ref cap sz bs =
  writeIORef ref EHS { ehs_cap = cap, ehs_remaining = sz, ehs_bs = bs }

-- Turns an instance of Get into a Decoder
decoder :: Get a -> Decoder a
decoder = runGetIncremental

-- Given a Decoder, repeadetly reads input from Handle h in chunks of size
-- sz bytes until the output is produced or fails.
parseIncremental  :: Decoder (Result a) -> Handle -> Int -> B.ByteString -> IO (Result a, ByteOffset, B.ByteString)
parseIncremental dec hdl sz bs = parseIncremental' (dec `pushChunk` bs) hdl sz
    
parseIncremental' :: Decoder (Result a) -> Handle -> Int -> IO (Result a, ByteOffset, B.ByteString)
parseIncremental' (Fail bs size _)   hdl sz = return (Incomplete, size, bs)  
parseIncremental' (Done bs size dat) hdl sz = return (dat, size, bs)
parseIncremental' dec@(Partial k)    hdl sz = do
    ch <- B.hGetSome hdl sz
    if ch == B.empty      
      then parseIncremental' (pushEndOfInput dec) hdl sz
      else parseIncremental' (dec `pushChunk` ch) hdl sz

-- Given a Handle, initialises a corresponding EventHandle
openEventHandle :: Handle -> IO EventHandle
openEventHandle handle = do
    -- Reading in 1MB chunks, not sure whether it's a resonable default
    -- Discard offset because it doesn't matter for headers
    (header, _, bs) <- parseIncremental (decoder getHeader) handle 1024 B.empty
    let imap = M.fromList [ (fromIntegral (num t),t) | t <- eventTypes (resultToHeader header)]
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
        dec = decoder (runReaderT (getEventIncremental parsers) parsers)
    ioref <- newIORef $ EHS { ehs_cap=Nothing, ehs_remaining=0, ehs_bs=bs }
    return $ EH handle dec ioref


-- Gets a single Event, nothing if no more events are available (either EOF
-- or log is incomplete)
readEvent :: EventHandle -> IO (Result CapEvent)
readEvent (eh@EH { eh_decoder = dec, eh_hdl = hdl, eh_state = ref})
  = do 
    EHS { ehs_cap = cap, ehs_remaining = bytes_remaining, ehs_bs = bs } <- readIORef ref
    if bytes_remaining > 0
      then do -- we are in the middle of an EventBlock
        (!ei, !size, !bs') <- parseIncremental dec hdl chunkSize bs
        case ei of 
          One ev -> do
            updateEHS ref cap (bytes_remaining - (fromIntegral size)) bs'
            return $ One (CapEvent { ce_cap = cap, ce_event = ev })
          Incomplete -> do
            -- Nothing was read so same bytes_remaining but all the remaining input
            -- is now in bs' (it was all consumed but not enough to finish a parse)
            --readIORef ref >>= print 
            --updateEHS ref cap bytes_remaining bs'
            --readIORef ref >>= print 
            return Incomplete
          Complete -> return Complete
      else do -- we are out of an EventBlock
        (!ei, _, !bs')   <- parseIncremental dec hdl chunkSize bs
        updateEHS ref Nothing 0 bs'
        case ei of 
          One ev -> do
            case (spec ev) of
              -- Should probably use the timestamp for chrono
              EventBlock _ block_cap new_sz -> do
                -- Global EventBlocks have block_cap=65535
                let new_cap = if (fromIntegral block_cap /= ((-1) :: Word16))
                              then Just block_cap
                              else Nothing
                updateEHS ref new_cap new_sz bs'
                readEvent eh
              otherwise  -> return $ One (CapEvent { ce_cap = Nothing, ce_event = ev })
          Incomplete -> do
            updateEHS ref Nothing 0 bs'
            return Incomplete
          Complete -> return Complete

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