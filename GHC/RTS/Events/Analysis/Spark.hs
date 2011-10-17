module GHC.RTS.Events.Analysis.Spark
  ( SparkState
  , sparkMachine
  )
 where

import GHC.RTS.Events

--------------------------------------------------------------------------------
data SparkState
  = SparkInitial  -- ^ Spark initial state
  | SparkCreated  -- ^ Spark created, but not yet running
  | SparkRunning  -- ^ Spark converted and running

-- | This state machine tracks the progress of a single spark
sparkMachine :: Machine SparkState EventInfo
sparkMachine = Machine
  { initial = SparkInitial
  , final   = sparkFinal
  , alpha   = sparkAlpha
  , delta   = sparkDelta
  }
 where
  sparkFinal SparkInitial = True
  sparkFinal _            = False

  sparkAlpha SparkCreate    = True
  sparkAlpha SparkDud       = True
  sparkAlpha SparkOverflow  = True
  sparkAlpha SparkRun       = True
  sparkAlpha (SparkSteal _) = True
  sparkAlpha SparkFizzle    = True
  sparkAlpha SparkGC        = True
  sparkAlpha _              = False

  -- SparkInitial
  sparkDelta SparkInitial SparkCreate   = Just SparkCreated
  sparkDelta SparkInitial SparkDud      = Just SparkInitial
  sparkDelta SparkInitial SparkOverflow = Just SparkInitial
  -- SparkCreated
  sparkDelta SparkCreated SparkRun       = Just SparkRunning
  sparkDelta SparkCreated (SparkSteal _) = Just SparkCreated
  sparkDelta SparkCreated SparkFizzle    = Just SparkInitial
  -- SparkRunning
  sparkDelta SparkRunning SparkFizzle = Just SparkInitial
  sparkDelta SparkRunning SparkGC     = Just SparkInitial

  sparkDelta _ _ = Nothing

