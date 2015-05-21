{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE DataKinds           #-}
module Control.Disruptor.Math where
import Data.Bits
import Data.Proxy
import GHC.TypeLits

newtype PowerOf (n :: Nat) = Power { power :: Word }
  deriving (Show)

nTo :: forall a n. (KnownNat n, Integral a) => PowerOf (n :: Nat) -> a
nTo p = fromIntegral (natVal p) ^ fromIntegral (power p)

logBaseN :: forall a n. (KnownNat n, Integral a) => a -> Maybe (PowerOf (n :: Nat))
logBaseN x = go 1 $ divMod x base
  where
    base = (fromIntegral $ natVal (Proxy :: Proxy n)) :: a
    go c (1, 0) = Just $ Power c
    go c (n, 0) = go (c + 1) $ divMod n base
    go _ _      = Nothing

ceilingNextPowerOfTwo :: (Num b, FiniteBits b) => b -> Int
ceilingNextPowerOfTwo x = 1 `shiftL` (finiteBitSize x - countLeadingZeros (x - 1))
{-# INLINE ceilingNextPowerOfTwo #-}

-- TODO figure out if it's possible to get this to use
-- the bsr instruction instead (maybe try getting it into ghc-prim / Data.Bits)
-- Or maybe try inline-c for this to use __asm__.
logBase2 :: (FiniteBits b) => b -> Int
logBase2 x = (finiteBitSize x - 1) - countLeadingZeros x
{-# INLINE logBase2 #-}

