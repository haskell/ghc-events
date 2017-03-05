{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module GHC.RTS.Events.Incremental
  ( -- * Incremental API
    Decoder(..)
  , decodeHeader
  , decodeEvents
  , decodeEventLog

  -- * Lazy API
  , readHeader
  , readEvents
  , readEventLog

  -- * Legacy API
  , readEventLogFromFile
  ) where
import Control.Applicative
import Control.Monad
import Data.Either
import Data.Maybe
import Data.Monoid
import Data.Word
import Prelude

import qualified Data.Binary.Get as G
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Internal as BL

import GHC.RTS.Events
import GHC.RTS.EventsIncremental (mkEventDecoder)

data Decoder a
  = Consume (B.ByteString -> Decoder a)
  | Produce !a (Decoder a)
  | Done B.ByteString
  | Error B.ByteString String

pushChunk :: Decoder a -> B.ByteString -> Decoder a
pushChunk decoder chunk = case decoder of
  Consume k -> k chunk
  Produce a decoder' -> Produce a $ decoder' `pushChunk` chunk
  Done leftover -> Done $ leftover `B.append` chunk
  Error leftover err -> Error (leftover `B.append` chunk) err

withHeader :: (Header -> B.ByteString -> Decoder r) -> Decoder r
withHeader f = go $ G.runGetIncremental getHeader
  where
    go decoder = case decoder of
      G.Done leftover _ header -> f header leftover
      G.Partial k -> Consume $ \chunk -> go $ k $ Just chunk
      G.Fail leftover _ err -> Error leftover err

decodeHeader :: Decoder Header
decodeHeader = withHeader $ \header leftover -> Produce header $ Done leftover

decodeEvents :: Header -> Decoder Event
decodeEvents header = go (0 :: Int) Nothing decoder0
  where
    decoder0 = mkEventDecoder header
    go !remaining !blockCap decoder = case decoder of
      G.Done leftover consumed r -> do
        let !decoder' = decoder0 `G.pushChunk` leftover
        case r of
          Just event -> case evSpec event of
            EventBlock {..} ->
              go (fromIntegral block_size) (mkCap cap) decoder'
            _ -> do
              let
                !remaining' = remaining - fromIntegral consumed
                !blockCap' = if remaining' > 0 then blockCap else Nothing
                !event' = event { evCap = blockCap }
              Produce event' $ go remaining' blockCap' decoder'
          Nothing -> go remaining blockCap decoder'
      G.Partial k ->
        Consume $ \chunk -> go remaining blockCap $ k $ Just chunk
      G.Fail leftover _ err ->
        Error leftover err
    mkCap cap = do
      guard $ fromIntegral cap /= (-1 :: Word16)
      return cap

decodeEventLog :: Decoder Event
decodeEventLog = withHeader $ \header leftover ->
  decodeEvents header `pushChunk` leftover

readHeader :: BL.ByteString -> Either String (Header, BL.ByteString)
readHeader = go $ Left decodeHeader
  where
    go r bytes = case r of
      Left decoder -> case decoder of
        Produce header decoder' -> case decoder' of
          Done leftover -> Right (header, BL.Chunk leftover bytes)
          _ -> fail "readHeader: unexpected decoder"
        Consume k -> case bytes of
          BL.Empty -> fail "readHeader: not enough bytes"
          BL.Chunk chunk chunks -> go (Left $! k chunk) chunks
        Done _ -> fail "readHeader: unexpected termination"
        Error _ err -> fail err
      Right header -> Right (header, bytes)

readEvents :: Header -> BL.ByteString -> ([Event], Maybe String)
readEvents header = f . go (decodeEvents header)
  where
    f :: [Either e a] -> ([a], Maybe e)
    f xs = (rights rs, listToMaybe (lefts ls))
      where
        (rs, ls) = break isLeft xs
#if !MIN_VERSION_base(4, 7, 0)
        isLeft (Left _) = True
        isLeft _ = False
#endif
    go :: Decoder Event -> BL.ByteString -> [Either String Event]
    go decoder bytes = case decoder of
      Produce event decoder' -> Right event : go decoder' bytes
      Consume k -> case bytes of
        BL.Empty -> []
        BL.Chunk chunk chunks -> go (k chunk) chunks
      Done {} -> []
      Error _ err -> [Left err]

readEventLog :: BL.ByteString -> Either String (EventLog, Maybe String)
readEventLog bytes = do
  (header, bytes') <- readHeader bytes
  case readEvents header bytes' of
    (events, err) -> return (EventLog header (Data events), err)

readEventLogFromFile :: FilePath -> IO (Either String EventLog)
readEventLogFromFile path = fmap fst . readEventLog <$> BL.readFile path
