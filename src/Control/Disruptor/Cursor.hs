module Control.Disruptor.Cursor where
import Control.Disruptor.Sequence

class Cursor c where
  getCursor :: c -> IO (SequenceId a)

