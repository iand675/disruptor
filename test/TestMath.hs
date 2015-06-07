module TestMath where
import           Control.Disruptor.Math
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.QuickCheck

prop_findNextPower_exactPowersOfTwo :: Property
prop_findNextPower_exactPowersOfTwo = property $ forAll (suchThat arbitrary (> 0)) $ check
  where
    check :: Word -> Bool
    check x = (2 ^ logBase2 x) == ceilingNextPowerOfTwo ((2 ^ logBase2 x) :: Word)

prop_findNextPower_nextHigherPowersOfTwo = property $ forAll (suchThat arbitrary (> 0)) $ check
  where
    check :: Word -> Bool
    check x = (ceilingNextPowerOfTwo (((2 ^ logBase2 x) + 1) :: Word)) == 2 ^ (logBase2 x + 1)

mathTests =
  [ testProperty "ceilingNextPowerOfTwo exact powers of two" prop_findNextPower_exactPowersOfTwo
  , testProperty "ceilingNextPowerOfTwo non-powers of two" prop_findNextPower_nextHigherPowersOfTwo
  ]

