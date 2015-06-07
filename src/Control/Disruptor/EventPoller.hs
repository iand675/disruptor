module Control.Disruptor.EventPoller where
import Control.Disruptor.DataProvider

data PollState = Processing | Gating | Idle

data EventPoller p sequencer sequence gatingSequence a = EventPoller
  { eventPollerDataProvider    :: p
  , eventPollerSequencer       :: sequencer
  , eventPollerSequence        :: sequence
  , eventPollerGatingSequence  :: gatingSequence
  }

eventPoller :: (DataProvider p a, GetSequence sequence, SetSequence sequence, GetSequence gatingSequence)
            => p
            -> sequencer
            -> sequence
            -> gatingSequence
            -> EventPoller p sequencer sequence gatingSequence a
eventPoller = EventPoller

type Handler e a = a -> SequenceId a -> Bool -> IO (Checked e ())

poll poller handler = do
  currentSequence <- get $ eventPollerSequence poller
  let nextSequence = succ currentSequence
  availableSequence <- (eventPollerSequencer poller) nextSequence =<< get (eventPollerGatingSequence poller)
  undefined
