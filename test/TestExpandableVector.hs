module TestExpandableVector where
import           Data.Foldable
import qualified Data.Vector.Expandable as E
import qualified Data.Vector.Expandable.Mutable as EM
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Generic as V
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

prop_expandableCons :: [Int] -> Property
prop_expandableCons xs = monadicIO $ do
  vec <- run $ M.new 0
  vec' <- run $ foldrM EM.cons vec xs
  assert (length xs == M.length vec')
  frozen <- run $ E.freezeFixed vec'
  assert (M.length vec' == V.length frozen)
  assert (V.toList frozen == xs)

prop_expandableSnoc :: [Int] -> Property
prop_expandableSnoc xs = monadicIO $ do
  vec <- run $ M.new 0
  vec' <- run $ foldlM EM.snoc vec xs
  assert (length xs == M.length vec')
  frozen <- run $ E.freezeFixed vec'
  assert (M.length vec' == V.length frozen)
  assert (V.toList frozen == xs)

expandableVectorTests =
  [ testProperty "cons" prop_expandableCons
  , testProperty "snoc" prop_expandableSnoc
  ]

