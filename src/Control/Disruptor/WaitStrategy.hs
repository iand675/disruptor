{-# LANGUAGE ViewPatterns #-}
module Control.Disruptor.WaitStrategy where
import Control.Concurrent
import qualified Control.Concurrent.Condition as Condition
import qualified Control.Concurrent.ReentrantLock as Lock
import Control.Disruptor.Sequence
import Control.Disruptor.SequenceBarrier

class WaitStrategy s where
  waitFor :: s
          -> SequenceId c -- ^ Desired sequence value
          -> Sequence -- ^ cursor
          -> SequenceReader c -- ^ dependent sequence
          -> SequenceBarrier c
          -> IO (Either WaitResult (SequenceId c))

  signalAllWhenBlocking :: s -> IO ()
  signalAllWhenBlocking = const $ return ()

data BlockingWait = BlockingWait
  { blockingWaitLock      :: Lock.ReentrantLock
  , blockingWaitCondition :: Condition.Condition Lock.ReentrantLock
  }

{-
withBlock 

defaultBlockingWait = BlockingWait

instance WaitStrategy BlockingWait where
  waitFor (conf lock condition) desired cursor dependent barrier = go
    where
      go = do
        available <- get cursor
        if available < desired
          then do
            let getSequence = do
                  available' <- get cursor
                  if available' < desired
                    then do
                      Event.wait condition
            lock $ do
-}

{-
lightBlockingWait
phasedBackoffWait
-}

data BusySpinWait = BusySpinWait
defaultBusySpinWait = BusySpinWait

instance WaitStrategy BusySpinWait where
  waitFor conf desired _ dependent barrier = go
    where
      go = do
        available <- readSequence dependent
        if available < desired
          then do
            a <- checkAlert barrier
            case a of
              Nothing -> go
              Just a -> return $! Left $! Alert a
          else return $ Right available

data SleepingWait = SleepingWait { sleepRetries :: Int }
defaultSleepingWait = SleepingWait 200

instance WaitStrategy SleepingWait where
  waitFor conf desired _ dependent barrier = go $ sleepRetries conf
    where
      go count = do
        available <- readSequence dependent
        if available < desired
          then do
            a <- checkAlert barrier
            case a of
              Nothing ->
                case count of
                  ((> 100) -> True) -> go $ pred count
                  ((> 0) -> True) -> do
                    yield
                    go $ pred count
                  _ -> threadDelay 1 >> go (pred count)
              Just a -> return $! Left $! Alert a
          else return $ Right available

data YieldingWait = YieldingWait { yieldRetries :: Int }
defaultYieldingWait = YieldingWait 100

instance WaitStrategy YieldingWait where
  waitFor conf desired _ dependent barrier = go $ yieldRetries conf
    where
      go count = do
        available <- readSequence dependent
        if available < desired
          then do
            a <- checkAlert barrier
            case a of
              Nothing ->
                case count of
                  0 -> yield >> go count
                  _ -> go (pred count)
              Just a -> return $! Left $! Alert a
          else return $ Right available
{-
timeoutBlockingWait
-}
