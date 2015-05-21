module Control.Concurrent.Lock.Internal where

data Acquisition l = Acquisition
  { release :: IO ()
  }

