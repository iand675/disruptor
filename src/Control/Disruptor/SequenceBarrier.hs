module Control.Disruptor.SequenceBarrier where
import           Control.Disruptor.Sequence
import qualified Data.Foldable as F
import           Data.IORef

-- TODO figure out if we need this at all
data AlertException = AlertException
alertException = AlertException

data WaitResult = Alert AlertException
                | Interrupted
                | Timeout

data SequenceBarrier c = SequenceBarrier
  { waitFor :: SequenceId c -> IO (Either WaitResult (SequenceId c))
  , getCursor :: IO (SequenceId c)
  , alert :: IO ()
  , clearAlert :: IO ()
  -- no need for isAlert because we have Maybe
  , checkAlert :: IO (Maybe AlertException)
  }

