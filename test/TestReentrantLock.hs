module TestReentrantLock where
import           Control.Concurrent.ReentrantLock
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.QuickCheck

test_lockThenUnlock = do
  lock <- run new
  mAcq <- run $ tryAcquire lock
  assert $ isJust mAcq
  case mAcq of
    Nothing -> return ()
    Just acq -> do
      release acq
  assert . not =<< run $ locked lock

reentrantLockTests =
  [ testProperty "acquire then release" prop_lockThenUnlock
  -- , testProperty "acquire, await, signal" prop_lockWithCondition
  ]
