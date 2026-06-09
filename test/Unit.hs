{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import GHC.RTS.Events (EventInfo(..), Event(..), Header(..), Data(..), serialiseEventLog, EventLog(..), EventType(..))
import Data.Binary.Put (runPut)
import Data.ByteString (ByteString)
import GHC.RTS.Events.Incremental (readEvents, readHeader, readEventLog)
import qualified Data.ByteString.Lazy as BL
import GHC.RTS.Events.Binary (putEvent)

main :: IO ()
main = hspec $ do
  describe "NonMovingHeapCensus" $ do
    it "can roundtrip logarithmic version" $ do
      roundtrips [EventType {num = 207, desc = "Nonmoving heap census", size = Just 13}] NonmovingHeapCensus
        { nonmovingCensusBlkSize = 8
        , nonmovingCensusActiveSegs = 1
        , nonmovingCensusFilledSegs = 2
        , nonmovingCensusLiveBlocks = 3
        , encodedAsLog = True
        }
    it "can roundtrip non-logarithmic version" $ do
      roundtrips [EventType {num = 207, desc = "Nonmoving heap census", size = Just 14}] NonmovingHeapCensus
        { nonmovingCensusBlkSize = 8
        , nonmovingCensusActiveSegs = 1
        , nonmovingCensusFilledSegs = 2
        , nonmovingCensusLiveBlocks = 3
        , encodedAsLog = False
        }

roundtrips :: [EventType] -> EventInfo -> IO ()
roundtrips header evInfo = do
  let event = Event 0 evInfo Nothing
  let enc = runPut $ putEvent event
  ([dec], _) <- pure $ readEvents (Header header) enc
  event `shouldBe` dec
