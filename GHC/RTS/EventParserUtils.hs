{-# LANGUAGE CPP, PatternGuards #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module GHC.RTS.EventParserUtils (
        EventParsers(..),
        GetEvents,
        GetHeader,

        getE,
        getH,
        getString,
        mkEventTypeParsers,
        skip,
    ) where

import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Data.Array
import Data.Binary
import Data.Binary.Get hiding (skip)
import qualified Data.Binary.Get as G
import Data.Binary.Put
import Data.Char
import Data.Function
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.List

#define EVENTLOG_CONSTANTS_ONLY
#include "EventLogFormat.h"

import GHC.RTS.EventTypes

-- reader/Get monad that passes around the event types
type GetEvents a = ReaderT EventParsers (ErrorT String Get) a

newtype EventParsers = EventParsers (Array Int (GetEvents EventTypeSpecificInfo))

type GetHeader a = ErrorT String Get a

getH :: Binary a => GetHeader a
getH = lift get

getE :: Binary a => GetEvents a
getE = lift $ lift get

nBytes :: Integral a => a -> GetEvents [Word8]
nBytes n = replicateM (fromIntegral n) getE

getString :: Integral a => a -> GetEvents String
getString len = do
    bytes <- nBytes len
    return $ map (chr . fromIntegral) bytes

skip :: Integral a => a -> GetEvents ()
skip n = lift $ lift $ G.skip (fromIntegral n)

--
-- Code to build the event parser table.
--

-- Our event log format allows new fields to be added to events over
-- time.  This means that our parser must be able to handle:
--
--  * old versions of an event, with fewer fields than expected,
--  * new versions of an event, with more fields than expected
--
-- The event log file declares the size for each event type, so we can
-- select the correct parser for the event type based on its size.  We
-- do this once after parsing the header: given the EventTypes, we build 
-- an array of event parsers indexed by event type.
-- 
-- For each event type, we may have multiple parsers for different
-- versions of the event, indexed by size.  These are listed in the
-- eventTypeParsers list below.  For the given log file we select the
-- parser for the most recent version (largest size less than the size
-- declared in the header).  If this is a newer version of the event
-- than we understand, there may be extra bytes that we have to read
-- and discard in the parser for this event type.
--
-- Summary:
--   if size is smaller that we expect:
--     parse the earier version, or ignore the event
--   if size is just right:
--     parse it
--   if size is too big:
--     parse the bits we understand and discard the rest

type VariableEventParsers = IntMap (GetEvents EventTypeSpecificInfo)

mkEventTypeParsers :: IntMap EventType
                   -> Array Int [(EventTypeSize, GetEvents EventTypeSpecificInfo)]
                   -> VariableEventParsers
                   -> Array Int (GetEvents EventTypeSpecificInfo)
mkEventTypeParsers etypes eventTypeParsers variableEventTypeParsers
 = accumArray (flip const) undefined (0, max_event_num)
    ([ (num, undeclared_etype num) | num <- [0..max_event_num] ] ++
     [ (num, parser num etype) | (num, etype) <- M.toList etypes ])
  where
    max_event_num = maximum (M.keys etypes)
    undeclared_etype num = throwError ("undeclared event type: " ++ show num)

    parser num etype =
         let
             possible
               | not (inRange (bounds eventTypeParsers) num) = []
               | otherwise = eventTypeParsers ! num
             mb_et_size = size etype
         in
         case mb_et_size of
           Nothing -> case M.lookup num variableEventTypeParsers of
                        Nothing -> noEventTypeParser num mb_et_size
                        Just p  -> p

           -- special case for GHC 6.12 EVENT_STOP_THREAD.  GHC 6.12
           -- was mis-reporting the event sizes (ThreadIds were
           -- counted as 8 instead of 4), and when we expanded the
           -- EVENT_STOP_THREAD to include an extra field, the new
           -- size is the same as that reported by 6.12, so we can't
           -- tell them apart by size.  Hence the special case here
           -- checks the size of the EVENT_CREATE_THREAD event to see
           -- whether we should be parsing the 6.12 STOP_THREAD or the
           -- 7.2 STOP_THREAD.  If the CREATE_THREAD extended in the
           -- future this might go wrong.

           Just et_size
             | et_size == sz_old_tid + 2,
               num == EVENT_STOP_THREAD,
                Just et <- M.lookup EVENT_CREATE_THREAD etypes,
                size et == Just sz_old_tid ->
                do  -- (thread, status)
                  t <- getE
                  s <- getE :: GetEvents Word16
                  let stat = fromIntegral s
                  return StopThread{thread=t, status = if stat > maxBound
                                                          then NoStatus
                                                          else mkStopStatus stat}

           Just et_size ->
             case [ (sz,p) | (sz,p) <- possible, sz <= et_size ] of
               [] -> noEventTypeParser num mb_et_size
               ps -> let (sz, best) = maximumBy (compare `on` fst) ps
                     in  if sz == et_size
                            then best
                            else do r <- best
                                    skip (et_size - sz)
                                    return r

noEventTypeParser :: Int -> Maybe EventTypeSize
                  -> GetEvents EventTypeSpecificInfo
noEventTypeParser num mb_size = do
  bytes <- case mb_size of
             Just n  -> return n
             Nothing -> getE :: GetEvents Word16
  skip bytes
  return UnknownEvent{ ref = fromIntegral num }

