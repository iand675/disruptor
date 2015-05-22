{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
module Control.Disruptor.RingBuffer.Generic where
import Control.Disruptor.Cursor
import Control.Disruptor.DataProvider
import Control.Disruptor.Sequence
import Control.Disruptor.Sequencer

class (DataProvider (b a) a, Cursor (b a) a, Sequenced (b a) a) => RingBuffer (b :: * -> *) a where

