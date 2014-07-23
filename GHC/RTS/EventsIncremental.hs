{- 
 -  Incremental parser functions for GHC RTS EventLog framewrok.
 -}

 module GHC.RTS.EventsIncremental (
  EventHandle,
  openEventHandle,
  parseIncremental,
  getHdr,
  readEvent,
 ) where

import GHC.RTS.Events 
import GHC.RTS.EventParserUtils

import qualified Data.ByteString as B
import Data.Binary.Get 
import System.IO (Handle, hIsEOF, IOMode(..), withFile)
import System.Exit (exitFailure)

-- Datatype that holds the reference to an eventlog
data EventHandle = EventHandle Header Handle

-- Turns an instance of Get into a Decoder
decoder :: Get a -> Decoder a
decoder = runGetIncremental

-- Given a Decoder, repeadetly reads input from Handle h in chunks of size
-- sz bytes until the output is produced or fails.
parseIncremental  :: Decoder a -> Handle -> Int -> IO a
parseIncremental dec h sz = do
    -- Using hGetSome _should_ block only on empty handles where EOF is not
    -- yet reached.
    ch <- B.hGetSome h sz
    -- However, testing with incomplete headers did not terminate, therefore
    -- the latter.
    let dec' =  if ch == B.empty
                then pushEndOfInput dec
                else dec `pushChunk` ch
    case dec' of 
      (Fail _ _ errMsg) -> putStrLn errMsg >> exitFailure
      (Partial cont)    -> parseIncremental dec' h sz
      (Done _ _ dat)    -> return dat

-- Given a Handle, initialises a corresponding EventHandle
openEventHandle :: Handle -> IO EventHandle
openEventHandle h = do
    -- Reading in 1MB chunks, not sure whether it's a resonable default
    hdr <- parseIncremental (decoder getHeader) h 1024
    return $ EventHandle hdr h

-- Debug function
getHdr (EventHandle h _) = h

-- Gets a single Event, nothing if no more events are available (either EOF
-- or log is incomplete)
readEvent  :: EventHandle -> IO (Maybe Event)
readEvent h = undefined
