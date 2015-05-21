{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Vector.Expandable.Mutable where
import Control.Monad.Primitive
-- import Data.Primitive.MutVar
import           Data.Bits
import qualified Data.Vector.Generic.Mutable as G
import qualified Data.Vector.Mutable as MV

defaultGrowthStrategy :: Int -> Int
defaultGrowthStrategy = shiftL 1

data MVector s a = MVector {-# UNPACK #-} !Int -- public offset
                           {-# UNPACK #-} !Int -- public length
                           -- inner vector has *real length*,
                           -- always should have an offset of 0
                           {-# UNPACK #-} !(MV.MVector s a)
                           {-# UNPACK #-} !(Int -> Int)

instance G.MVector MVector a where
  basicLength (MVector _ l _ _) = l

  basicUnsafeSlice j m (MVector i n v f) = MVector (i + j) m v f

  {-# INLINE basicOverlaps #-}
  basicOverlaps (MVector i m v1 _) (MVector j n v2 _)
    = G.basicOverlaps (G.basicUnsafeSlice i m v1) (G.basicUnsafeSlice j n v2)

  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew n = MVector 0 n <$> G.basicUnsafeNew n <*> pure defaultGrowthStrategy

  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead (MVector i _ v _) j = G.basicUnsafeRead v (i + j)

  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (MVector i _ v _) j x = G.basicUnsafeWrite v (i + j) x

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MVector io no (MV.MVector i n dst) _) (MVector jo mo (MV.MVector j m src) _)
    = G.basicUnsafeCopy (MV.MVector (io + i) no dst) (MV.MVector (jo + j) mo src)

cons :: (PrimMonad m) => a -> MVector (PrimState m) a -> m (MVector (PrimState m) a)
cons x (MVector o l v resize) = do
  let original = MV.unsafeSlice 0 l v
  let actualLength = MV.length v
  baseTarget <- if actualLength == l
    then MV.new $ resize actualLength
    else return v
  let target = MV.unsafeSlice 1 l baseTarget
  MV.move original target
  MV.write baseTarget 0 x
  return $ MVector o (1 + l) baseTarget resize

unsafeUnexpand :: MVector s a -> MV.MVector s a
unsafeUnexpand (MVector io lo (MV.MVector i l arr) _) = MV.MVector (io + i) lo arr

-- snoc :: (PrimMonad m) => MVector (PrimMonad m) a -> a -> m ()
-- insert :: (PrimMonad m) => Int -> a -> MVector (PrimMonad m) a -> m ()
-- delete :: (PrimMonad m) => Int -> MVector (PrimMonad m) a -> m ()

