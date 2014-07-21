{- 
 -  Incremental parser functions for GHC RTS EventLog framewrok.
 -}

 module GHC.RTS.EventsIncremental (
  EventHandle,
  openEventHandle,
  getHeaderIncremental,
  getEvent,
  getHdr

 ) where

import GHC.RTS.Events 
import GHC.RTS.EventParserUtils

import qualified Data.ByteString as B
import Data.Binary.Get 
import System.IO (Handle, hIsEOF, IOMode(..), withFile)
import System.Exit (exitFailure)

data EventHandle = EventHandle Header Handle

getHeaderIncremental  :: Handle -> IO Header
getHeaderIncremental h = do
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
   hdr <- getHeaderIncremental h
   return $ EventHandle hdr h

getHdr :: EventHandle -> Header
getHdr (EventHandle h _) = h

getEvent  :: EventHandle -> IO (Maybe Event)
getEvent h = undefined
