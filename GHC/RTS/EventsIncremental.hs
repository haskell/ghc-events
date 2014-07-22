{- 
 -  Incremental parser functions for GHC RTS EventLog framewrok.
 -}

 module GHC.RTS.EventsIncremental (
  EventHandle,
  openEventHandle,
  parseHeaderIncremental,
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

parseHeaderIncremental  :: Handle -> IO Header
parseHeaderIncremental h = do
    loop (runGetIncremental getHeader)
     where
       loop (Fail _ _ errMsg) = putStrLn errMsg >> exitFailure
       loop (Partial k)       = chunk >>= loop . k
       loop (Done _ _ hdr)    = return hdr
 
       chunk = do
       isEof <- hIsEOF h
       if isEof
           then return Nothing
           else do  dat <- B.hGetSome h 128;
                    return $ Just dat

openEventHandle :: Handle -> IO EventHandle
openEventHandle h = do
   hdr <- parseHeaderIncremental h
   return $ EventHandle hdr h

getHdr (EventHandle h _) = h

readEvent  :: EventHandle -> IO (Maybe Event)
readEvent h = undefined
