module TestSequence where
import           Control.Concurrent
import qualified Control.Disruptor.Sequence as Sequence
import           Data.Int
import           Data.List (sort)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.QuickCheck

prop_deterministic_setGetSequence val = monadicIO $ do
  let sid = Sequence.SequenceId val
  seq <- run $ Sequence.makeSequence
  run $ Sequence.set seq sid
  val' <- run $ Sequence.get seq
  assert (sid == val')

prop_setVolatileGetSequence val = monadicIO $ do
  let sid = Sequence.SequenceId val
  seq <- run $ Sequence.makeSequence
  run $ Sequence.setVolatile seq sid
  val' <- run $ Sequence.get seq
  assert (sid == val')

prop_deterministic_addAndGetSequence start add = monadicIO $ do
  seq <- run $ Sequence.makeSequence' start
  val <- run $ Sequence.addAndGet seq add
  assert (Sequence.SequenceId (start + add) == val)

prop_sameSequenceIsSame = monadicIO $ do
  seq <- run $ Sequence.makeSequence
  assert $ Sequence.sameSequence seq seq

prop_differentSequenceIsNotSame = monadicIO $ do
  seq1 <- run $ Sequence.makeSequence
  seq2 <- run $ Sequence.makeSequence
  assert $ not $ Sequence.sameSequence seq1 seq2

prop_minimumSequence :: [Int64] -> Property
prop_minimumSequence ints = monadicIO $ do
  seqs <- run $ mapM Sequence.makeSequence' ints
  minSeq <- run $ Sequence.findMinimumSequence seqs
  if null ints
    then assert (minSeq == Sequence.SequenceId maxBound)
    else assert (Sequence.SequenceId (minimum ints) == minSeq)

prop_compareAndSetSequence :: [Int64] -> Property
prop_compareAndSetSequence ints = monadicIO $ do
  seq <- run $ Sequence.makeSequence
  resultVars <- run $ mapM (swapValues seq) ints
  -- just to take things out of the original order
  results <- run $ mapM takeMVar $ reverse resultVars
  final <- run $ Sequence.get seq
  assert (sort (map Sequence.SequenceId (Sequence._INITIAL_VALUE : ints)) == sort (final : results))
  where
    swapValues seq int = do
      mFinal <- newEmptyMVar
      let async = do
            x <- Sequence.get seq
            success <- Sequence.compareAndSet seq x (Sequence.SequenceId int)
            if success
              then putMVar mFinal x
              else async
      forkIO async
      return mFinal

sequenceTests =
  [ testProperty "set and get" prop_deterministic_setGetSequence
  , testProperty "add and get" prop_deterministic_addAndGetSequence
  , testProperty "volatile get" prop_setVolatileGetSequence
  , testProperty "CAS" prop_compareAndSetSequence
  , testProperty "sameness valid"prop_sameSequenceIsSame
  , testProperty "sameness invalid" prop_differentSequenceIsNotSame
  , testProperty "minimum" prop_minimumSequence
  ]
