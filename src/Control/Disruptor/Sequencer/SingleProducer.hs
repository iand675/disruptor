{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Disruptor.Sequencer.SingleProducer where
import Control.Concurrent
import Control.Disruptor.Cursor
import Control.Disruptor.Math
import Control.Disruptor.Sequence
import Control.Disruptor.SequenceBarrier.Processing
import Control.Disruptor.Sequencer
import Control.Disruptor.Sink (InsufficientCapacity(..))
import Control.Disruptor.WaitStrategy
import Control.Monad
import Data.CheckedException
import Data.IORef
import qualified Data.Vector as V

data SingleProducer w a = SingleProducer
  { producerBufferSize :: !(PowerOf 2)
  , cursor             :: Sequence
  , gatingSequences    :: V.Vector Sequence
  , waitStrategy       :: w
  , nextValue          :: IORef (SequenceId a)
  , cachedValue        :: IORef (SequenceId a)
  }

instance (WaitStrategy w) => Sequenced (SingleProducer w a) a where
  bufferSize = producerBufferSize

  hasAvailableCapacity p req = do
    nextValue <- readIORef $ nextValue p
    let wrapPoint = (nextValue + SequenceId req) - (nTo $ bufferSize p)
    cachedGatingSequence <- readIORef $ cachedValue p
    if (wrapPoint > cachedGatingSequence) || (cachedGatingSequence > nextValue)
      then do
        minSequence <- findMinimumSequence' nextValue $ gatingSequences p
        writeIORef (cachedValue p) minSequence
        return (wrapPoint < minSequence)
      else return True

  remainingCapacity s = do
    produced <- readIORef $ nextValue s
    consumed <- findMinimumSequence' produced $ gatingSequences s
    return $ fromIntegral ((nTo $ bufferSize s) - fromIntegral (produced - consumed))

  next s = snd <$> nextN s 1

  nextN s n = do
    -- TODO handle n <= 0
    nextVal <- readIORef $ nextValue s
    let nextSequence = nextVal + SequenceId n
    let wrapPoint = nextSequence - (nTo $ bufferSize s)
    cachedGatingSequence <- readIORef $ cachedValue s
    when (wrapPoint > cachedGatingSequence || cachedGatingSequence > nextVal) $ do
      let go = do
            minSequence <- findMinimumSequence' nextVal $ gatingSequences s
            if wrapPoint > minSequence
              then threadDelay 1 >> go -- TODO LockSupport.parkNanos(1) in Java, is this equivalent?
              else writeIORef (cachedValue s) minSequence
      go
    writeIORef (nextValue s) nextSequence
    return (nextVal, nextSequence)

  tryNext s = tryNextN s 1

  tryNextN s n = do
    hasCapacity <- hasAvailableCapacity s n
    if hasCapacity
      then do
        current <- readIORef (nextValue s)
        let next = current + SequenceId n
        writeIORef (nextValue s) next
        ok next
      else report InsufficientCapacity

  publish p s = set (cursor p) s >> signalAllWhenBlocking (waitStrategy p)

  publishN p (_, hi) = publish p hi

instance Cursor (SingleProducer w a) a where
  getCursor = get . cursor

instance (WaitStrategy w) => Sequencer (SingleProducer w a) a where
  claim p = writeIORef (nextValue p)
  isAvailable p s = (s <=) <$> get (cursor p)
  -- addGatingSequences
  -- removeGatingSequences
  newBarrier s = processingSequenceBarrier s (waitStrategy s) (cursor s)
