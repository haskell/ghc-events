{-# OPTIONS_GHC -funbox-strict-fields #-}

import GHC.RTS.Events
import System.Environment
import Data.Monoid
import Data.List (foldl')
import Data.Word (Word32, Word16)

-- todo, proper error handling
main = do
    [out, file1, file2] <- getArgs
    Right f1 <- readEventLogFromFile file1
    Right f2 <- readEventLogFromFile file2
    let m = merge f1 f2
    writeEventLogToFile out m

{-
GHC numbers caps and capsets in sequential order, starting at 0.  Threads are
similarly numbered, but start at 1.  In order to merge logs 'x' and 'y', we
find the maximum values of each variable type in 'x', then increment each
variable in 'y' that amount.  This guarantees that variables in each log don't
clash, and that the meaning of each reference to a thread/cap/capset is
preserved.
-}

merge :: EventLog -> EventLog -> EventLog
merge (EventLog h1 (Data xs)) (EventLog h2 (Data ys)) | h1 == h2
 = EventLog h1 . Data . mergeOn time xs $ shift (maxVars xs) ys
merge _ _ = error "can't merge eventlogs with non-matching headers"

mergeOn :: Ord b => (a -> b) -> [a] -> [a] -> [a]
mergeOn f [] ys = ys
mergeOn f xs [] = xs
mergeOn f (x:xs) (y:ys) | f x <= f y = x : mergeOn f xs (y:ys)
                        | otherwise  = y : mergeOn f (x:xs) ys

data MaxVars = MaxVars { mcapset :: !Word32
                       , mcap :: !Int
                       , mthread :: !ThreadId }

instance Monoid MaxVars where
    -- threads start at 1
    mempty  = MaxVars 0 0 1
    mappend (MaxVars a b c) (MaxVars x y z) = MaxVars (max a x) (max b y) (max c z)
    -- avoid space leaks:
    mconcat = foldl' mappend mempty

-- Capabilities are represented as Word16.  An event block marked with the
-- capability of 0xffff belongs to no capability
isCap :: Int -> Bool
isCap x = fromIntegral x /= ((-1) :: Word16)

maxVars :: [Event] -> MaxVars
maxVars = mconcat . map (maxSpec . spec)
 where
    -- only checking binding sites right now, sufficient?
    maxSpec (Startup n) = mempty { mcap = n - 1 }
    maxSpec (CreateThread t) = mempty { mthread = t }
    maxSpec (CreateSparkThread t) = mempty { mthread = t }
    maxSpec (CapsetCreate cs _) = mempty {mcapset = cs }
    maxSpec (EventBlock _ _ es) = maxVars es
    maxSpec _  = mempty

sh :: Num a => a -> a -> a
sh x y = x + y + 1

shift :: MaxVars -> [Event] -> [Event]
shift mv@(MaxVars mcs mc mt) = map (\(Event t s) -> Event t $ shift' s)
 where
    -- -1 marks a block that isn't attached to a particular capability
    shift' (EventBlock et c bs) = EventBlock et (msh c) $ shift mv bs
     where msh x = if isCap x then sh mc x else x
    shift' (CreateThread t) = CreateThread $ sh mt t
    shift' (RunThread t) = RunThread $ sh mt t
    shift' (StopThread t s) = StopThread (sh mt t) s
    shift' (ThreadRunnable t) = ThreadRunnable $ sh mt t
    shift' (MigrateThread t c) = MigrateThread (sh mt t) (sh mc c)
    shift' (RunSpark t) = RunSpark (sh mt t)
    shift' (StealSpark t c) = StealSpark (sh mt t) (sh mc c)
    shift' (CreateSparkThread t) = CreateSparkThread (sh mt t)
    shift' (WakeupThread t c) = WakeupThread (sh mt t) (sh mc c)
    shift' (CapsetCreate cs cst) = CapsetCreate (sh mcs cs) cst
    shift' (CapsetDelete cs) = CapsetDelete (sh mcs cs)
    shift' (CapsetRemoveCap cs c) = CapsetRemoveCap (sh mcs cs) (sh mc c)
    shift' (RtsIdentifier cs rts) = RtsIdentifier (sh mcs cs) rts
    shift' (ProgramArgs cs as) = ProgramArgs (sh mcs cs) as
    shift' (ProgramEnv cs es) = ProgramEnv (sh mcs cs) es
    shift' (OsProcessPid cs pid) = OsProcessPid (sh mcs cs) pid
    shift' (OsProcessParentPid cs ppid) = OsProcessParentPid (sh mcs cs) ppid
    shift' x = x
