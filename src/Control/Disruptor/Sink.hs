{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Control.Disruptor.Sink where
import Data.CheckedException
import Data.Data (Data(..))
import Data.Typeable (Typeable(..))
import GHC.Generics
import qualified Data.Vector as V
import Control.Disruptor.Sequence

-- | TODO figure out if there's an easy way to get rid if this with a representation
-- that doesn't have to switch on purity

{-
data Update a = Pure !(a -> a)
              | Mutate !(a -> IO ())
-}

type Update a = a -> (a -> IO ()) -> IO ()

-- | Occurs when ring buffer is too small to accomodate as many elements as are in f
data RingBufferTooSmall = RingBufferTooSmall
  deriving (Show, Read, Eq, Data, Typeable, Generic)

-- | Occurs when it is not possible to insert a value into
-- the ring buffer without it wrapping the consuming sequenes.
-- Used specifically when claiming the next sequence with 'tryNext' or 'tryNextN' fails.
data InsufficientCapacity = InsufficientCapacity
  deriving (Show, Read, Eq, Data, Typeable, Generic)

type StandardErrors = '[RingBufferTooSmall]
type CautiousErrors = InsufficientCapacity ': StandardErrors

class Sink s a | s -> a where
  publish        :: s -> Update a -> IO (Checked StandardErrors ())
  tryPublish     :: s -> Update a -> IO (Checked CautiousErrors ())
  -- | Unlike the Java implementation, the batch operations calculate how many spots to reserve by extrapolating
  -- out from the starting sequence and the length of 'f' to claim the number of events it needs in the 'RingBuffer'.
  -- TODO: This may have some small performance implications, find out!
  -- publishMany    :: (Foldable f) => s -> f (Update a) -> SequenceId a -> IO (Checked StandardErrors ())
  publishMany    :: s -> V.Vector (Update a) -> SequenceId a -> IO (Checked StandardErrors ())

  -- | Unlike the Java implementation, the batch operations calculate how many spots to reserve by extrapolating
  -- out from the starting sequence and the length of 'f' to claim the number of events it needs in the 'RingBuffer'.
  -- TODO: This may have some small performance implications, find out!
  -- tryPublishMany :: (Foldable f) => s -> f (Update a) -> SequenceId a -> IO (Checked CautiousErrors ())
  tryPublishMany :: s -> V.Vector (Update a) -> SequenceId a -> IO (Checked CautiousErrors ())

