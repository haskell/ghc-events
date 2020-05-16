-- | Constant-space sorting.
--
-- This module provides a routine for sorting events in constant-space via
-- on-disk merge sort.
module GHC.RTS.Events.Sort
  ( GHC.RTS.Events.Sort.sortEvents
  ) where

import Data.Traversable
import Data.Coerce
import Data.Function (on)
import Data.List (sortBy, minimumBy)
import Data.Maybe
import Data.Foldable (toList)
import System.IO
import System.Directory
import Prelude

import Data.Binary.Put as P
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Sequence as S

import GHC.RTS.Events
import GHC.RTS.Events.Binary (putEventLog)

type SortedChunk = FilePath

-- | The chunk size which the input eventlog is broken into (in events). This
-- determines the upper-bound on memory usage during the sorting process.
--
-- This value is a reasonable trade-off between memory and computation,
-- requiring approximately 100MBytes while sorting a "typical" eventlog.
cHUNK_SIZE :: Int
cHUNK_SIZE = 500*1000

-- | Maximum number of chunks to merge at once. Determined by the largest
-- number of file descriptors we can safely open at once.
fAN_IN :: Int
fAN_IN = 256

newtype OnTime = OnTime Event

instance Ord OnTime where
  compare = coerce (compare `on` evTime)

instance Eq OnTime where
  (==) = coerce ((==) `on` evTime)

-- | @sortEvents tmpDir outPath eventlog@ sorts @eventlog@ via on-disk merge
-- sort, using @tmpDir@ for intermediate data. The sorted eventlog is written
-- to @eventlog@.
sortEvents :: FilePath  -- ^ temporary directory
           -> FilePath  -- ^ output eventlog file path
           -> EventLog  -- ^ eventlog to sort
           -> IO ()
sortEvents _tmpDir _outPath (EventLog _ (Data [])) = fail "sortEvents: no events"
sortEvents tmpDir outPath (EventLog hdr (Data events0)) = do
    chunks <- toSortedChunks events0
    hdl <- openBinaryFile outPath WriteMode
    mergeChunks' hdl chunks
    hClose hdl
    return ()
  where
    toSortedChunks :: [Event] -> IO (S.Seq SortedChunk)
    toSortedChunks =
      fmap S.fromList
      . mapM (writeTempChunk . sortEventsInMem)
      . chunksOf cHUNK_SIZE

    mergeChunks' :: Handle -> S.Seq SortedChunk -> IO ()
    mergeChunks' destFile chunks
      | S.null chunks =
        fail "sortEvents: this can't happen"
      | S.length chunks <= fAN_IN = do
        events <- mapM readChunk chunks
        let sorted = mergeSort $ toList (coerce events :: S.Seq [OnTime])
        writeChunk destFile (coerce sorted)
        mapM_ removeFile chunks
        hClose destFile
      | otherwise = do
        chunksss <- flip mapM (nChunks fAN_IN chunks) $ \fps -> do
          (fp, hdl) <- createTempChunk
          mergeChunks' hdl fps
          return fp
        mergeChunks' destFile (S.fromList chunksss)

    readChunk :: SortedChunk -> IO [Event]
    readChunk fp = do
      result <- readEventLogFromFile fp
      case result of
        Left err -> fail $ "sortEvents: error reading chunk: " ++ fp ++ ": " ++ err
        Right (EventLog _ (Data events)) -> return events

    createTempChunk :: IO (FilePath, Handle)
    createTempChunk =
      openBinaryTempFile tmpDir "chunk"

    writeTempChunk :: [Event] -> IO FilePath
    writeTempChunk evs = do
      (fp, hdl) <- createTempChunk
      writeChunk hdl evs
      hClose hdl
      return fp

    writeChunk :: Handle -> [Event] -> IO ()
    writeChunk hdl events =
      BSL.hPutStr hdl
      $ P.runPut
      $ putEventLog
      $ EventLog hdr
      $ Data events

-- | An unordered set.
type Bag a = [a]

-- | Break a list in chunks of the given size.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
  let (ys, rest) = splitAt n xs
   in ys : chunksOf n rest

-- | Break a 'S.Seq' into \(n\) roughly-even chunks.
nChunks :: Int -> S.Seq a -> [S.Seq a]
nChunks n xs0 = go xs0
  where
    go :: S.Seq a -> [S.Seq a]
    go xs
      | S.null xs = []
      | otherwise = let (x,y) = S.splitAt len xs in x : go y
    len = S.length xs0 `div` n + 1

-- | Merge the given lists into sorted order.
mergeSort :: Ord a => Bag [a] -> [a]
mergeSort = go
  where
    go [] = []
    go xss =
      case catMaybes $ mapZipper f xss of
        [] -> []
        xs -> minimumBy (compare `on` head) xs

    f :: Ord a => Bag [a] -> [a] -> Maybe [a]
    f _    [] = Nothing
    f rest (x:xs) = Just $ x : go (xs : rest)

mapZipper :: (Bag a -> a -> b) -> Bag a -> [b]
mapZipper f = go []
  where
    --go :: Bag a -> Bag [a] -> [b]
    go _prevs [] = []
    go prevs (x:nexts) =
      f (prevs ++ nexts) x : go (x : prevs) nexts

sortEventsInMem :: [Event] -> [Event]
sortEventsInMem =
  sortBy (compare `on` evTime)

