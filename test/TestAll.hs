module Main where
import Control.Disruptor.Math
import Data.Foldable
import Data.Int
import Data.List (sort)
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic
import TestMath
import TestSequence
import TestExpandableVector
import TestLock
-- import TestReentrantLock

main = defaultMain $ testGroup "Suites"
  [ testGroup "Control.Disruptor.Sequence" sequenceTests
  , testGroup "Data.Vector.Expandable" expandableVectorTests
  , testGroup "Control.Disruptor.Math" mathTests
  , testGroup "Control.Concurrent.Lock" lockTests
  ]

