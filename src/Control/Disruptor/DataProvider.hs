{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Disruptor.DataProvider where
import Control.Disruptor.Sequence

class DataProvider p a | p -> a where
  get :: p -> SequenceId a -> IO a

