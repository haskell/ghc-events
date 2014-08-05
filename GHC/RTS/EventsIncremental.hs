{-# LANGUAGE CPP,BangPatterns,PatternGuards #-}
{-# OPTIONS_GHC -funbox-strict-fields -fwarn-incomplete-patterns #-}
{- 
 -  Incremental parser functions for GHC RTS EventLog framewrok.
 -}

 module GHC.RTS.EventsIncremental (
  EventHandle,
  openEventHandle,
  readEvent,
  printEventsIncremental
 ) where

import GHC.RTS.Events 
import GHC.RTS.EventParserUtils
import GHC.RTS.EventTypes

import Control.Monad.Reader
import qualified Data.ByteString as B
import qualified Data.IntMap as M
import Data.Binary.Get 
import Data.IORef (IORef, readIORef, writeIORef, newIORef)
import Debug.Trace (traceShow)
import System.IO (Handle, hIsEOF, IOMode(..), withFile, hTell)
import System.Exit (exitFailure)

#define EVENTLOG_CONSTANTS_ONLY
#include "EventLogFormat.h"

-- Datatype that holds the reference to an eventlog
data EventHandle
  = EH { eh_hdl :: Handle
       , eh_parsers :: EventParsers
       , eh_state :: IORef EventHandleState }

data EventHandleState
  = EHS { ehs_cap       :: Maybe Int
        , ehs_remaining :: Integer  -- Integer is n_remaining
        , ehs_bs        :: B.ByteString -- Un-read part of input, before where eh_hdl points
        }

newEHS :: B.ByteString -> EventHandleState
newEHS bs = EHS { ehs_cap = Nothing, ehs_remaining = 0, ehs_bs = bs }

-- Turns an instance of Get into a Decoder
decoder :: Get a -> Decoder a
decoder = runGetIncremental

-- Given a Decoder, repeadetly reads input from Handle h in chunks of size
-- sz bytes until the output is produced or fails.
parseIncremental  :: Decoder a -> Handle -> Int -> B.ByteString -> IO (a, B.ByteString)
parseIncremental dec h sz bs = do
    ch <- if bs == B.empty
          then B.hGetSome h sz
          else return bs
    -- However, testing with incomplete headers did not terminate, therefore
    -- the latter.
    let dec' =  if ch == B.empty
                then pushEndOfInput dec
                else dec `pushChunk` ch
    case dec' of 
      (Fail _ _ errMsg) -> putStrLn errMsg >> exitFailure
      (Partial cont)    -> parseIncremental dec' h sz B.empty
      (Done bs _ dat)    -> return (dat, bs)

-- Given a Handle, initialises a corresponding EventHandle
openEventHandle :: Handle -> IO EventHandle
openEventHandle handle = do
    -- Reading in 1MB chunks, not sure whether it's a resonable default
    (header, bs) <- parseIncremental (decoder getHeader)          handle 1024 B.empty
    -- (_, bs')     <- parseIncremental (decoder getDataBeginMarker) handle 1024 bs
    let imap = M.fromList [ (fromIntegral (num t),t) | t <- eventTypes header]
        -- This test is complete, no-one has extended this event yet and all future
        -- extensions will use newly allocated event IDs.
        is_ghc_6 = Just sz_old_tid == do create_et <- M.lookup EVENT_CREATE_THREAD imap
                                         size create_et
        {-
        -- GHC6 writes an invalid header, we handle it here by using a
        -- different set of event parsers.  Note that the ghc7 event parsers
        -- are standard events, and can be used by other runtime systems that
        -- make use of threadscope.
        -}
        event_parsers = if is_ghc_6
                            then standardParsers ++ ghc6Parsers
                            else standardParsers ++ ghc7Parsers
                                 ++ mercuryParsers ++ perfParsers
        parsers = mkEventTypeParsers imap event_parsers
    ioref <- newIORef . newEHS $ bs
    return $ EH handle (EventParsers parsers) ioref

-- Gets a single Event, nothing if no more events are available (either EOF
-- or log is incomplete)
readEvent :: EventHandle -> IO (Maybe CapEvent)
readEvent (eh@EH { eh_parsers = parsers, eh_hdl = hdl, eh_state = ref})
  = do 
      EHS { ehs_cap = cap, ehs_remaining = bytes_remaining, ehs_bs = bs } <- readIORef ref
      if bytes_remaining > 0
        then do -- we are in the middle of an EventBlock
          !event_start <- hTell hdl
          (!ei, !bs') <- getEventInfo parsers hdl bs
          !event_end   <- hTell hdl
          writeIORef ref (EHS { ehs_cap = cap
                              -- TODO: make this accurate
                              , ehs_remaining = bytes_remaining - (event_end - event_start)
                              , ehs_bs = bs' })
          case ei of 
            Just ev -> return $ Just (CapEvent { ce_cap = cap, ce_event = ev })
            Nothing -> return Nothing
          -- return $ Just (CapEvent { ce_cap = cap, ce_event = ei })
        else do -- we have finished the previous chunk of events
          -- TODO: do we want this here?
          --writeIORef ref (EHS { ehs_cap = Nothing
          --                    , ehs_remaining = 0
          --                    , ehs_bs = bs})
          !event_start <- hTell hdl
          (!ei, !bs')   <- getEventInfo parsers hdl bs
          !event_end   <- hTell hdl
          --print (B.length bs)
          --print (B.length bs')
          writeIORef ref (EHS { ehs_cap = cap
                              -- TODO: make this accurate
                              , ehs_remaining = bytes_remaining - (event_end - event_start)
                              , ehs_bs = bs' })
          case ei of 
            Just ev ->
                case (spec ev) of
                  EventBlock _ new_cap new_sz -> do
                      writeIORef ref (EHS { ehs_cap = Just new_cap
                                          , ehs_remaining = new_sz
                                          , ehs_bs = bs'})
                      readEvent eh
                  otherwise  -> return $ Just (CapEvent { ce_cap = Nothing, ce_event = ev })
            Nothing -> return Nothing

printEventsIncremental :: EventHandle -> IO ()
printEventsIncremental eh = do
    evt <- readEvent eh
    let dbg = False
    case evt of 
      Just ev -> do
          print ev
          input <- if dbg 
                      then getLine
                      else return ""
          if input == ""
            then printEventsIncremental eh
            else putStrLn "Stopping"
      Nothing -> do putStrLn "Done"

--getEventInfo :: Handle -> B.ByteString -> IO (Maybe Event, B.ByteString)
getEventInfo :: EventParsers -> Handle -> B.ByteString -> IO (Maybe Event, B.ByteString)
getEventInfo parsers hdl unparsed_bs
  = do
      (ev, bs) <- parseIncremental (decoder (runReaderT (getEvent parsers) parsers)) hdl 1024 unparsed_bs
      return (ev, bs)



---- newtype Capability = Int

---- | An event annotated with the Capability that generated it, if any
--data CapEvent
--  = CapEvent { ce_cap   :: Maybe Int,
--               ce_event :: Event
--               -- we could UNPACK ce_event, but the Event constructor
--               -- might be shared, in which case we could end up
--               -- increasing the space usage.
--             } deriving Show

--data Event =
--  Event {
--    time :: {-# UNPACK #-}!Timestamp,
--    spec :: EventInfo
--  } deriving Show

--data EventParser a
--    = FixedSizeParser {
--        fsp_type :: EventTypeId,
--        fsp_size :: EventTypeSize,
--        fsp_parser :: GetEvents a
--    }
--    | VariableSizeParser {
--        vsp_type :: EventTypeId,   -- Event identifier
--        vsp_parser :: GetEvents a
--    }

--type EventTypeId = Int
---- (Get a) is defined in Binary

--type GetEvents a = ReaderT EventParsers Get a
--  -- Allows read-only access to the EventParsers

--newtype EventParsers = EventParsers (Array EventTypeId (GetEvents EventInfo))
--  -- The EventInfo parser for each event type