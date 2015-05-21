module Control.Concurrent.Condition (
  Condition (..)
) where
import Control.Concurrent.Acquisition

data Condition l = Condition
  { await :: IO ()
  -- , awaitMask
  -- , awaitTimeout
  -- , awaitUntil
  , signal :: {- Acquisition l -> -} IO ()
  , signalAll :: {- Acquisition l -> -} IO ()
  , destroy :: IO ()
  }

