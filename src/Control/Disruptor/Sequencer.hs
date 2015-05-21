{-# LANGUAGE DataKinds #-}
module Control.Disruptor.Sequencer where
import Control.Disruptor.Cursor
import Control.Disruptor.Math
import Control.Disruptor.Sequence
import Control.Disruptor.SequenceBarrier
import Data.Foldable
import Data.Int

class Sequenced a where
  getBufferSize :: a -> IO (PowerOf 2)
  hasAvailableCapacity :: a -> Int64 -> IO Bool
  remainingCapacity :: a -> IO Int64
  next :: a -> IO (SequenceId c)
  nextN :: a -> Int64 -> IO (SequenceRange c)
  tryNext :: a -> IO (Maybe (SequenceId c))
  tryNextN :: a -> Int64 -> IO (Maybe (SequenceId c))
  publish :: a -> SequenceId c -> IO ()
  publishN :: a -> SequenceRange c -> IO ()

-- IDEA Would be cool to use RankNTypes to ensure that the appropriate publisher is using the right SequenceId?

class (Cursor a, Sequenced a) => Sequencer a where
  claim :: a -> SequenceId c -> IO ()
  isAvailable :: a -> SequenceId c -> IO Bool
  addGatingSequences :: Foldable f => a -> f Sequence -> IO ()
  removeGatingSequence :: a -> Sequence -> IO Bool
  newBarrier :: (Foldable f) => a -> f Sequence -> IO (SequenceBarrier c)
  getMinimumSequence :: a -> IO (SequenceId c)
  getHighestPublishedSequence :: a -> SequenceId c -> SequenceId c -> IO (SequenceId c)
  -- newPoller
