{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
module Control.Disruptor.Sequencer where
import Control.Disruptor.Cursor
import Control.Disruptor.Math
import Control.Disruptor.Sequence
import Control.Disruptor.SequenceBarrier
import Control.Disruptor.Sink
import Data.Foldable
import Data.CheckedException
import Data.Int

class Sequenced s a | s -> a where
  bufferSize :: s -> PowerOf 2
  hasAvailableCapacity :: s -> Int64 -> IO Bool
  remainingCapacity :: s -> IO Int64
  next :: s -> IO (SequenceId a)
  nextN :: s -> Int64 -> IO (SequenceRange a)
  tryNext :: ('[InsufficientCapacity] :< e) => s -> IO (Checked e (SequenceId a))
  tryNextN :: ('[InsufficientCapacity] :< e) => s -> Int64 -> IO (Checked e (SequenceId a))
  publish :: s -> SequenceId a -> IO ()
  publishN :: s -> SequenceRange a -> IO ()

-- IDEA Would be cool to use RankNTypes to ensure that the appropriate publisher is using the right SequenceId?

class (Cursor s a, Sequenced s a) => Sequencer s a | s -> a where
  claim :: s -> SequenceId a -> IO ()
  isAvailable :: s -> SequenceId a -> IO Bool
  addGatingSequences :: Foldable f => s -> f Sequence -> IO ()
  removeGatingSequence :: s -> Sequence -> IO Bool
  newBarrier :: (Foldable f) => s -> f Sequence -> IO (SequenceBarrier a)
  getMinimumSequence :: s -> IO (SequenceId a)
  getHighestPublishedSequence :: s -> SequenceId a -> SequenceId a -> IO (SequenceId a)
  -- newPoller
