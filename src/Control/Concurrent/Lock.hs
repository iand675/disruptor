module Control.Concurrent.Lock (
  Lock,
  Acquisition,
  Condition,
  new,
  newAcquired,
  acquire,
  release,
  registerCondition,
  await,
  signal,
  signalAll,
  destroy
) where

import Control.Concurrent
import Control.Concurrent.Condition
import Control.Concurrent.Lock.Internal
import Control.Exception
import qualified Data.Vector as V

data Conditions = Conditions
  { lastNotified :: Int
  , conditions   :: V.Vector (MVar ())
  }

signalImpl :: Lock -> IO ()
signalImpl lock = mask_ $ do
  conds <- takeMVar $ lockConditions lock
  let cs = conditions conds
      notifyIndex = (1 + lastNotified conds) `mod` V.length cs
      chosen = V.unsafeIndex (conditions conds) notifyIndex
  tryPutMVar chosen ()
  putMVar (lockConditions lock) (conds { lastNotified = notifyIndex })

signalAllImpl :: Lock -> IO ()
signalAllImpl lock = mask_ $ do
  -- Considered using readMVar, but don't want to allow
  -- conditions to be concurrently added while signalling?
  conds <- takeMVar $ lockConditions lock
  V.forM_ (conditions conds) (flip tryPutMVar ())
  putMVar (lockConditions lock) conds

data Lock = Lock { lock           :: MVar ()
                 , lockConditions :: MVar Conditions
                 }

new :: IO Lock
new = Lock <$> newMVar () <*> newMVar (Conditions 0 V.empty)

newAcquired :: IO (Lock, Acquisition Lock)
newAcquired = do
  l <- new
  a <- acquire l
  return (l, a)

acquire :: Lock -> IO (Acquisition Lock)
acquire l = do
  val <- takeMVar $ lock l
  return $ Acquisition (putMVar (lock l) val)

tryAcquire :: Lock -> IO (Maybe (Acquisition Lock))
tryAcquire l = do
  mVal <- tryTakeMVar $ lock l
  return $ case mVal of
    Nothing -> Nothing
    Just val -> Just $ Acquisition (putMVar (lock l) val)

locked :: Lock -> IO Bool
locked = isEmptyMVar . lock

registerCondition :: Lock -> IO (Condition Lock)
registerCondition l = mask_ $ do
  conds <- takeMVar $ lockConditions l
  newCond <- newEmptyMVar
  putMVar (lockConditions l) $ conds { conditions = V.snoc (conditions conds) newCond }
  let destroyImpl = modifyMVarMasked_ (lockConditions l) $ \laterConds -> do
        putMVar newCond ()
        let updatedConditions = V.filter (/= newCond) (conditions laterConds)
        return $ laterConds { conditions = updatedConditions
                            , lastNotified = if lastNotified laterConds > V.length updatedConditions
                                               then 0
                                               else lastNotified laterConds
                            }
      -- release lock, block until signal, then reacquire lock
      awaitImpl = mask_ (putMVar (lock l) () >> takeMVar newCond >> takeMVar (lock l))
  return $ Condition awaitImpl (signalImpl l) (signalAllImpl l) destroyImpl


