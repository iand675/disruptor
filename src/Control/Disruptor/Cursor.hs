{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Disruptor.Cursor where
import Control.Disruptor.Sequence

class Cursor c a | c -> a where
  getCursor :: c -> IO (SequenceId a)

