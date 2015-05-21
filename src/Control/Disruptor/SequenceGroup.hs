module Control.Disruptor.SequenceGroup where
import           Control.Disruptor.Sequence
import           Control.Monad
import           Data.Atomics
import           Data.IORef
import qualified Data.Vector as V

data SequenceGroup = SequenceGroup
  { sequenceGroupSequences :: IORef (V.Vector Sequence)
  }

instance GetSequence SequenceGroup where
  get = (readIORef . sequenceGroupSequences) >=> findMinimumSequence
  {-# INLINEABLE get #-}

instance SetSequence SequenceGroup where
  set g x = V.mapM_ (flip set x) =<< readIORef (sequenceGroupSequences g)
  {-# INLINEABLE set #-}

add :: SequenceGroup -> Sequence -> IO ()
add g s = atomicModifyIORefCAS_ (sequenceGroupSequences g) (flip V.snoc s)

remove :: SequenceGroup -> Sequence -> IO Bool -- ^ True if sequence was removed
remove g s = atomicModifyIORefCAS (sequenceGroupSequences g) validateRemoval
  where
    validateRemoval v = let v' = V.filter (not . sameSequence s) v in (v, V.length v > V.length v')

size :: SequenceGroup -> IO Int
size g = V.length <$> readIORef (sequenceGroupSequences g)

-- TODO
-- addWhileRunning
