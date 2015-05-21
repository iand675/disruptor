module Control.Disruptor.Math where
import Data.Bits

ceilingNextPowerOfTwo :: (Num b, FiniteBits b) => b -> Int
ceilingNextPowerOfTwo x = 1 `shiftL` (finiteBitSize x - countLeadingZeros (x - 1))
{-# INLINE ceilingNextPowerOfTwo #-}

-- TODO figure out if it's possible to get this to use
-- the bsr instruction instead (maybe try getting it into ghc-prim / Data.Bits)
-- Or maybe try inline-c for this to use __asm__.
logBase2 :: (FiniteBits b) => b -> Int
logBase2 x = (finiteBitSize x - 1) - countLeadingZeros x
{-# INLINE logBase2 #-}

