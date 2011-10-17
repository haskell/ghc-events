module GHC.RTS.Events.Analysis.Thread 
  ( ThreadState (..)
  , threadMachine
  , threadIndexer
  )
 where

import GHC.RTS.Events
import GHC.RTS.Events.Analysis

--------------------------------------------------------------------------------
-- | This datatype defines the state machine for a single thread.
data ThreadState
  = ThreadInitial
  | ThreadQueued
  | ThreadRunning
  | ThreadStopped
  | ThreadFinal
  deriving (Show, Eq, Ord)

-- | This state machine tracks the events processed by a thread.
threadMachine :: Machine ThreadState EventInfo
threadMachine = Machine
  { initial = ThreadInitial
  , final   = threadFinal
  , alpha   = threadAlpha
  , delta   = threadDelta
  }
 where
  threadFinal ThreadFinal   = True
  threadFinal _             = False

  threadAlpha (CreateThread _)   = True
  threadAlpha (RunThread _)      = True
  threadAlpha (StopThread _ _)   = True
  threadAlpha (WakeupThread _ _) = True
  threadAlpha (ThreadRunnable _) = True
  threadAlpha _                  = False

  -- ThreadInitial
  threadDelta ThreadInitial (CreateThread _) = Just ThreadQueued
  -- ThreadQueued
  threadDelta ThreadQueued (RunThread _)      = Just ThreadRunning
  threadDelta ThreadQueued (ThreadRunnable _) = Just ThreadQueued
  threadDelta ThreadQueued (WakeupThread _ _) = Just ThreadQueued
  -- ThreadRunning
  threadDelta ThreadRunning (StopThread _ StackOverflow)  = Just ThreadQueued
  threadDelta ThreadRunning (StopThread _ HeapOverflow)   = Just ThreadQueued
  threadDelta ThreadRunning (StopThread _ ForeignCall)    = Just ThreadQueued
  threadDelta ThreadRunning (StopThread _ ThreadFinished) = Just ThreadFinal
  threadDelta ThreadRunning (StopThread _ _)              = Just ThreadStopped
  -- ThreadStopped
  threadDelta ThreadStopped (ThreadRunnable _) = Just ThreadQueued
  threadDelta ThreadStopped (WakeupThread _ _) = Just ThreadQueued
  -- Unknown
  threadDelta _ _ = Nothing

threadIndexer :: EventInfo -> Maybe ThreadId
threadIndexer event = case event of
  (CreateThread threadId)   -> Just threadId
  (RunThread threadId)      -> Just threadId
  (StopThread threadId _)   -> Just threadId
  (ThreadRunnable threadId) -> Just threadId
  (WakeupThread threadId _) -> Just threadId
  _                         -> Nothing

