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

data EventHandle = EventHandle Header Handle

decoder :: Get a -> Decoder a
decoder = runGetIncremental

parseIncremental  :: Decoder a -> Handle -> Int -> IO a
parseIncremental dec  h sz = do
    ch <- B.hGetNonBlocking h sz
    let dec' =  dec `pushChunk` ch
    case dec' of 
      (Fail _ _ errMsg) -> putStrLn errMsg >> exitFailure
      (Partial cont)    -> parseIncremental dec' h sz
      (Done _ _ dat)    -> return dat

openEventHandle :: Handle -> IO EventHandle
openEventHandle h = do
   hdr <- parseIncremental (decoder getHeader) h 1024
   return $ EventHandle hdr h

getHdr (EventHandle h _) = h

readEvent  :: EventHandle -> IO (Maybe Event)
readEvent h = undefined
