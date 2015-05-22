{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Disruptor.Sequence where
import Control.Monad.Primitive
import Data.Atomics
import qualified Data.Foldable as F
import Data.Int
import Data.Primitive
import Data.Primitive.Array

-- NOTE: Currently specialized for 64-bit architecture (assumes Int is 64-bit in size)
#ifdef x86_64_HOST_ARCH
#define FAST_MODE 1

import GHC.Int
import GHC.Prim
import GHC.Types

#define VALUE_OFFSET 7

#define SEQUENCE_BASE_TYPE \
  MutableByteArray (PrimState IO)
#define SEQUENCE_CREATE\
  newAlignedPinnedByteArray (_BUFFER_SIZE * 8) 16

#else

#define SEQUENCE_BASE_TYPE \
  MutableArray (PrimState IO) Int64
#define SEQUENCE_CREATE \
  newArray _BUFFER_SIZE

#endif

_VALUE_OFFSET = VALUE_OFFSET
_INITIAL_VALUE = -1
_BUFFER_SIZE = 15

class GetSequence a where
  -- | Perform a volatile read of 'Sequence' value
  get           :: a -> IO (SequenceId c)

newtype SequenceReader c = SequenceReader
  { readSequence :: IO (SequenceId c)
  }

sequenceReader :: GetSequence a => a -> SequenceReader c
sequenceReader = SequenceReader . get

class SetSequence a where
  -- | Perform an ordered write of this sequence. The intent is a
  -- Store/Store barrier between this write and any previous store.
  set           :: a -> (SequenceId c) -> IO ()

class (GetSequence a, SetSequence a) => RawSequence a where
  -- | Perform a volatile write of this sequence. The intent is
  -- a Store/Store barrier between this write and any previous
  -- write and a Store/Load barrier between this write and any
  -- subsequent volatile read.
  setVolatile   :: a -> (SequenceId c) -> IO ()
  compareAndSet :: a
                -> (SequenceId c)   -- ^ expected old value
                -> (SequenceId c)   -- ^ new value
                -> IO Bool -- Whether CAS succeeded
  incrementAndGet :: a -> IO (SequenceId c)
  addAndGet :: a -> Int64 -> IO (SequenceId c)

-- | A sequence is a padded, mutable 'Int64' in order to
-- prevent false sharing.
newtype Sequence = Sequence
  { sequenceInnards :: SEQUENCE_BASE_TYPE
  }

makeSequence :: IO Sequence
{-# INLINEABLE makeSequence #-}
makeSequence' :: Int64 -> IO Sequence
{-# INLINE makeSequence' #-}

#ifdef FAST_MODE
makeSequence = makeSequence' _INITIAL_VALUE

makeSequence' initialValue = do
  arr <- SEQUENCE_CREATE
  writeByteArray arr _VALUE_OFFSET initialValue
  return $ Sequence arr

instance GetSequence Sequence where
  get s = SequenceId <$> readByteArray (sequenceInnards s) _VALUE_OFFSET
  {-# INLINEABLE get #-}

instance SetSequence Sequence where
  set (Sequence (MutableByteArray arr#)) (SequenceId (I64# val#)) =
    IO (\st# -> (# (atomicWriteIntArray# arr# off# val# st#), () #))
    where !(I# off#) = VALUE_OFFSET
  {-# INLINEABLE set #-}

instance RawSequence Sequence where
  setVolatile s = writeByteArray (sequenceInnards s) _VALUE_OFFSET . fromSequenceId
  {-# INLINEABLE setVolatile #-}

  compareAndSet (Sequence (MutableByteArray arr#)) (SequenceId (I64# expect#)) (SequenceId (I64# new#)) = IO $ \st# ->
    let (# s2#, res# #) = (casIntArray# arr# off# expect# new# st#) in
    (# s2#, isTrue# (expect# ==# res#) #)
    where
      !(I# off#) = VALUE_OFFSET
  {-# INLINEABLE compareAndSet #-}

  incrementAndGet = flip addAndGet 1
  {-# INLINEABLE incrementAndGet #-}

  addAndGet (Sequence (MutableByteArray arr#)) (I64# incr#) = IO $ \ st# ->
    let (# s2#, res# #) = fetchAddIntArray# arr# off# incr# st#
    in
      -- Newer GHC versions (7.10-) return the old version, not the final val
#if MIN_VERSION_base(4, 8, 0)
      (# s2#, SequenceId (I64# (res# +# incr#)) #)
#else
      (# s2#, SequenceId (I64# res#) #)
#endif
    where
      !(I# off#) = VALUE_OFFSET
  {-# INLINEABLE addAndGet #-}

sameSequence :: Sequence -> Sequence -> Bool
sameSequence (Sequence (MutableByteArray a#)) (Sequence (MutableByteArray b#)) = isTrue# (sameMutableByteArray# a# b#)
{-# INLINEABLE sameSequence #-}

#endif

findMinimumSequence :: (F.Foldable f, GetSequence a) => f a -> IO (SequenceId c)
findMinimumSequence = findMinimumSequence' maxBound

findMinimumSequence' :: (F.Foldable f, GetSequence a) => SequenceId c -> f a -> IO (SequenceId c)
findMinimumSequence' = F.foldlM go
  where
    go cur seq = min cur <$> get seq

newtype SequenceId c = SequenceId { fromSequenceId :: Int64 }
  deriving (Read, Show, Eq, Ord, Enum, Num, Integral, Bounded, Real)

type SequenceRange c = (SequenceId c, SequenceId c)

