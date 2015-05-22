module Control.Disruptor.SequenceBarrier.Processing where
import Control.Disruptor.FixedSequenceGroup
import Control.Disruptor.Sequence
import Control.Disruptor.SequenceBarrier
import Control.Disruptor.Sequencer
import qualified Control.Disruptor.WaitStrategy as Wait
import qualified Data.Foldable as F
import Data.IORef

processingSequenceBarrier :: (F.Foldable f, Sequencer s a, Wait.WaitStrategy w) => s -> w -> Sequence -> f Sequence -> IO (SequenceBarrier a)
processingSequenceBarrier sequencer wait cursor dependents = do
  alertRef <- newIORef False
  return $ barrier alertRef
  where
    dependentSequence = if F.null dependents
                          then sequenceReader cursor
                          else sequenceReader $ fixedSequenceGroup dependents

    barrier alertRef = SequenceBarrier (waitForImpl alertRef) getCursorImpl (alertImpl alertRef) (clearAlertImpl alertRef) (checkAlertImpl alertRef)

    waitForImpl alertRef s = do
      mAlert <- checkAlertImpl alertRef
      case mAlert of
        Nothing -> do
          available <- Wait.waitFor wait s cursor dependentSequence (barrier alertRef)
          case available of
            Right s' -> if s' < s
              then return $ Right s'
              else Right <$> getHighestPublishedSequence sequencer s s'
            Left e -> return $ Left e
        Just a -> return $ Left $ Alert a

    getCursorImpl = readSequence dependentSequence

    alertImpl alertRef = writeIORef alertRef True >> Wait.signalAllWhenBlocking wait

    isAlertedImpl alertRef = readIORef alertRef

    clearAlertImpl alertRef = writeIORef alertRef False

    checkAlertImpl alertRef = readIORef alertRef >>= \a -> if a then return (Just alertException) else return Nothing

