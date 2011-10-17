module GHC.RTS.Events.Analysis
  ( Machine (..)
  , validate
  , simulate
  , Profile (..)
  , profile
  , runIndexed
  , refineInput
  , profiledMachine
  , indexedMachine
  )
 where

import GHC.RTS.Events

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, catMaybes)
import Data.Either (rights)

--------------------------------------------------------------------------------
-- | This is based on a simple finite state machine hence the names `delta`
-- for the state transition function.
-- Since states might be more than simple pattern matched constructors, we
-- use `finals :: state -> Bool`, rather than `Set state`, to indicate that
-- the machine is in some final state. Similarly for `alpha`, which
-- indicates the alphabet of inputs to a machine.
-- The function `delta` returns `Maybe` values, where `Nothing`
-- indicates that no valid transition is possible: ie, there has been an
-- error.
data Machine s i = Machine
  { initial :: s                 -- ^ Initial state
  , final   :: s -> Bool         -- ^ Valid final states
  , alpha   :: i -> Bool         -- ^ Valid input alphabet
  , delta   :: s -> i -> Maybe s -- ^ State transition function
  }

-- | As a simple example, the following machine takes numbers that are less
-- than 100, and has state True when it accepts an even number:
evenMachine :: Machine Bool Int
evenMachine = Machine
  { initial = True
  , final   = id
  , alpha   = (< 100)
  , delta   = \_ e -> Just $ even e
  }

-- | The `step` function runs a machine in a state against a single input. The
-- result is `Left state input` if some `state` failed for an `ìnput`, and
-- `Right state` for a successful state.
step :: Machine s i -> s -> i -> Either (s, i) s
step m s i
  | final m s = Right s
  | alpha m i = case delta m s i of
      Just s' -> Right s'
      Nothing -> Left (s, i)
  | otherwise = Right s

-- | The `validate` function takes a machine and a list of inputs. The machine
-- is started from its initial state and run agains the inputs in turn. If
-- there is an error, then Left is returned, otherwise Right.
validate :: Machine s i -> [i] -> Either (s, i) s
validate m = foldl (>>=) (Right (initial m)) . map (flip (step m))

-- | The `simulate` function keeps track of the states that the machine
-- goes through as it consumes input.
simulate :: Machine s i -> [i] -> [Either (s, i) s]
simulate m = scanl (>>=) (Right (initial m)) . map (flip (step m))

-- | A state augmented by Timestamp information is held in `profileState`.
-- When the state changes, `profileMap` stores a map between each state
-- and its cumulative time.
data Profile s = Profile
  { profileState :: (s, Timestamp)  -- ^ The current state and entry time
  , profileMap   :: Map s Timestamp -- ^ The cumulative time in each state
  }

-- | A profile is generated from a machine with a profiling state.
profile :: Eq s
        => Machine (Profile s) (i, Timestamp)
        -> [(i, Timestamp)]
        -> Map s Timestamp
profile machine = profileMap . either fst id . validate machine

-- | An indexed result is one where a set of identical indexed machines
-- are given input depending on an indexing function. The results are
-- then collected in a map, one per index.
runIndexed :: (Ord k, Show k, Show i)
           => (i -> Maybe k)            -- ^ An indexing function
           -> (Machine s i -> [i] -> r) -- ^ The evaluation function
           -> Machine s i               -- ^ The machine to index
           -> [i]                       -- ^ The list of inputs
           -> Map k r                   -- ^ Indexed results
runIndexed index eval machine is = M.map (eval machine)
                                 . M.fromListWith (++) . reverse -- Note: fromListWith reverses
                                 . catMaybes
                                 $ zipWith leftPrune ks is'
 where
  ks   = map index is
  is'  = map return is

  leftPrune :: Maybe k -> i -> Maybe (k, i)
  leftPrune (Just k) i = Just (k, i)
  leftPrune Nothing  _ = Nothing

-- | Machines sometimes need to operate on coarser input than they are defined
-- for. This function takes a function that refines input and a machine that
-- works on refined input, and produces a machine that can work on coarse input.
refineInput :: (i -> j) -> Machine s j -> Machine s i
refineInput refine machine = Machine
  { initial = initial machine
  , final   = final machine
  , alpha   = alpha machine . refine
  , delta   = \s -> delta machine s . refine
  }

--------------------------------------------------------------------------------
-- | This function takes a machine and profiles its state.
-- TODO: It seems a shame to lose all profiling data when something has gone
-- wrong. The profile itself might have type Maybe ...
profiledMachine :: Ord s => Machine s i -> Machine (Profile s) (i, Timestamp)
profiledMachine machine = Machine
  { initial = Profile (initial machine, 0) M.empty
  , final   = final machine . fst . profileState
  , alpha   = alpha machine . fst
  , delta   = profiledMachineDelta
  }
 where
  profiledMachineDelta (Profile (s, ts) m) (i, ts') = do
    s' <- delta machine s i
    return $ Profile (s', ts') (M.insertWith (+) s (ts' - ts) m)

-- | An indexed machine takes a function that multiplexes the input to a key
-- and then takes a machine description to an indexed machine.
indexedMachine :: Ord k
               => (i -> Maybe k)        -- ^ An indexing function
               -> Machine s i           -- ^ A machine to index with
               -> Machine (Map k s) i   -- ^ The indexed machine
indexedMachine index machine = Machine
  { initial = M.empty
  , final   = indexedMachineFinal
  , alpha   = indexedMachineAlpha
  , delta   = indexedMachineDelta
  }
 where
  -- The indexer is in a final state if all its elements are.
  indexedMachineFinal m = all (final machine) . M.elems $ m

  -- The alphabet of the indexer is that of its elements.
  indexedMachineAlpha = alpha machine

  -- If the index is not yet in the mapping, we start a new machine in its
  -- initial state. The indexer fails if indexed state fails.
  indexedMachineDelta m i = do
    k <- index i
    let state = fromMaybe (initial machine) (M.lookup k m)
    state' <- delta machine state i
    return $ M.insert k state' m

