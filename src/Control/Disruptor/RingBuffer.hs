{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE BangPatterns          #-}
module Control.Disruptor.RingBuffer where
import Control.Exception
import qualified Control.Disruptor.RingBuffer.Generic as G
import Control.Disruptor.Cursor
import Control.Disruptor.DataProvider
import Control.Disruptor.Sequence
import Control.Disruptor.Sequencer
import qualified Control.Disruptor.Sink as Sink
import Control.Disruptor.Math
import Data.Bits
import Data.CheckedException
import qualified Data.Foldable as F
import Data.Int
import Data.Primitive.Types
import GHC.Prim
import GHC.Types
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Mutable as M
import qualified Data.Vector as V

-- TODO figure out if these even matter in Haskell land (e.g. do buffer padding tricks matter?)
-- _SCALE = I# (sizeOf# (Addr nullAddr#))
-- _BUFFER_PAD = 128 `div` _SCALE
-- _REF_ELEMENT_SHIFT = logBase2 _SCALE
-- indexMask :: M.MVector s a -> Int64
-- indexMask = fromIntegral . pred . M.length

calculatedIndex :: SequenceId a -> M.MVector s a -> Int
calculatedIndex (SequenceId s) v = fromIntegral s `mod` M.length v

data RingBuffer s a = RingBuffer
  { ringBuffer         :: {-# UNPACK #-} !(M.IOVector a)
  , ringBufferSequence :: !s
  }

mkRingBuffer :: (Sequencer s) => s -> IO (RingBuffer s a)
mkRingBuffer s = do
  bufferPower <- getBufferSize s
  ringBuffer <- M.new $ nTo bufferPower
  return $ RingBuffer ringBuffer s

mkRingBuffer' :: (Sequencer s) => s -> IO a -> IO (RingBuffer s a)
mkRingBuffer' s m = do
  bufferPower <- getBufferSize s
  ringBuffer <- V.replicateM (nTo bufferPower) m >>= V.unsafeThaw
  bufferPower <- getBufferSize s
  return $ RingBuffer ringBuffer s

instance DataProvider (RingBuffer s a) a where
  get (RingBuffer b _) s = M.unsafeRead b (calculatedIndex s b)

-- | TODO get Cursor to force produced SequenceId to unify with 'a'
instance Cursor s => Cursor (RingBuffer s a) where
  getCursor = getCursor . ringBufferSequence

instance Sequenced s => Sequenced (RingBuffer s a) where
  getBufferSize = getBufferSize . ringBufferSequence
  hasAvailableCapacity = hasAvailableCapacity . ringBufferSequence
  remainingCapacity = remainingCapacity . ringBufferSequence
  next = next . ringBufferSequence
  nextN = nextN . ringBufferSequence
  tryNext = tryNext . ringBufferSequence
  tryNextN = tryNextN . ringBufferSequence
  publish = publish . ringBufferSequence
  publishN = publishN . ringBufferSequence

instance (Sequencer s) => Sequencer (RingBuffer s a) where
  claim = claim . ringBufferSequence
  isAvailable = isAvailable . ringBufferSequence
  addGatingSequences = addGatingSequences . ringBufferSequence
  removeGatingSequence = removeGatingSequence . ringBufferSequence
  newBarrier = newBarrier . ringBufferSequence
  getMinimumSequence = getMinimumSequence . ringBufferSequence
  getHighestPublishedSequence = getHighestPublishedSequence . ringBufferSequence

toVector :: Foldable t => t a -> V.Vector a
toVector = V.fromList . F.toList
{-# INLINE toVector #-}

atEnd :: IO b -> IO a -> IO a
atEnd = flip finally
{-# INLINE atEnd #-}

instance (Sequenced s) => Sink.Sink (RingBuffer s a) a where
  publish (RingBuffer b s) u = do
    sequence <- next s
    (atEnd $ publish s sequence) $ do
      let ix = calculatedIndex sequence b
      x <- M.unsafeRead b ix
      u x (M.unsafeWrite b ix)
    ok ()

  tryPublish (RingBuffer b s) u = do
    mSequence <- tryNext s
    case mSequence of
      Nothing -> report Sink.InsufficientCapacity
      Just sequence -> (atEnd $ publish s sequence) $ do
        let ix = calculatedIndex sequence b
        x <- M.unsafeRead b ix
        u x (M.unsafeWrite b ix)
        ok ()

  publishMany (RingBuffer b s) us total = do
    range@(lo, hi) <- nextN s $ fromIntegral $ V.length us
    (atEnd $ publishN s range) $ F.forM_ (V.indexed us) $ \(i, u) -> do
      let ix = calculatedIndex (lo + fromIntegral i) b
      x <- M.unsafeRead b ix
      u x (M.unsafeWrite b ix)
    ok ()

  tryPublishMany (RingBuffer b s) us total = do
    mHi <- tryNextN s $ fromIntegral $ V.length us
    case mHi of
      Nothing -> report Sink.InsufficientCapacity
      Just hi -> do
        let lo =  hi - fromIntegral (V.length us - 1)
        (atEnd $ publishN s (lo, hi)) $ F.forM_ (V.indexed us) $ \(i, u) -> do
          let ix = calculatedIndex (lo + fromIntegral i) b
          x <- M.unsafeRead b ix
          u x (M.unsafeWrite b ix)
        ok ()

type SequencedBuffer a = forall s. Sequencer s => RingBuffer s a

resetTo :: SequencedBuffer a -> SequenceId a -> IO ()
resetTo = undefined

claimAndGetPreallocated :: SequencedBuffer a -> SequenceId a -> IO a
claimAndGetPreallocated = undefined

isPublished :: SequencedBuffer a -> SequenceId a -> IO Bool
isPublished = undefined

