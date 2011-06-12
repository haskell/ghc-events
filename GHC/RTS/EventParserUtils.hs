
module GHC.RTS.EventParserUtils (
        GetEvents,
        EventParsers(..),
        GetHeader,

        getH,
        getE,
        getString,
        skip
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

