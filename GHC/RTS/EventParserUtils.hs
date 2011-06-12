
module GHC.RTS.EventParserUtils (
        GetEvents,
        EventParsers(..),
        GetHeader,

        getH,
        getE,
        getString
    ) where

import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Data.Array
import Data.Binary
import Data.Binary.Get
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

getString :: Integral a => a -> GetEvents String
getString len = do
    bytes <- replicateM (fromIntegral len) getE
    return $ map (chr . fromIntegral) (bytes :: [Word8])

