module Control.Concurrent.ReentrantLock where

import Control.Concurrent
import Control.Concurrent.Condition
import Control.Concurrent.Lock.Internal
import Control.Exception
import Control.Monad
import Data.Word
import qualified Data.Vector as V

data Conditions = Conditions
  { lastNotified :: Int
  , conditions   :: V.Vector (MVar ())
  }

data Owner
  = Owner
    { ownerThread    :: {-# UNPACK #-} !ThreadId
    , ownerLockCount :: {-# UNPACK #-} !Word64
    }
  | NoOwner

data ReentrantLock = Lock
  { lockOwner      :: MVar Owner
  , lockConditions :: MVar Conditions
  , innerLock      :: MVar ()
  }

new :: IO ReentrantLock
new = do
  inner <- newMVar ()
  owner <- newMVar NoOwner
  conds <- newMVar (Conditions 0 V.empty)
  return $ Lock owner conds inner

newAcquired :: IO (ReentrantLock, Acquisition ReentrantLock)
newAcquired = do
  l <- new
  a <- acquire l
  return (l, a)

acquire :: ReentrantLock -> IO (Acquisition ReentrantLock)
acquire (Lock mOwner mConds inner) = do
  tid <- myThreadId
  let innerAcquire = do
        owner <- takeMVar mOwner
        let a = Acquisition $ mask_ $ do
              tid' <- myThreadId
              modifyMVar_ mOwner $ \o -> case o of
                NoOwner -> error "Attempting to release lock that is not currently acquired"
                (Owner oTid c) -> do
                  if oTid /= tid'
                    then error "Attempting to release lock owned by other thread"
                    else do
                      let count' = pred c
                      when (count' == 0) $ putMVar inner ()
                      return $ Owner oTid count'
        case owner of
          NoOwner -> do
            takeMVar inner
            putMVar mOwner (Owner tid 1)
            return a
          Owner oTid c -> if tid == oTid
                            then do
                              putMVar mOwner (Owner tid (succ c))
                              return a
                            else do
                              putMVar mOwner owner
                              -- await the real lock being readable before contending for ownership again
                              readMVar inner
                              innerAcquire
  mask_ innerAcquire

signalImpl :: ReentrantLock -> IO ()
signalImpl lock = mask_ $ do
  conds <- takeMVar $ lockConditions lock
  let cs = conditions conds
      notifyIndex = (1 + lastNotified conds) `mod` V.length cs
      chosen = V.unsafeIndex (conditions conds) notifyIndex
  tryPutMVar chosen ()
  putMVar (lockConditions lock) (conds { lastNotified = notifyIndex })

signalAllImpl :: ReentrantLock -> IO ()
signalAllImpl lock = mask_ $ do
  -- Considered using readMVar, but don't want to allow
  -- conditions to be concurrently added while signalling?
  conds <- takeMVar $ lockConditions lock
  V.forM_ (conditions conds) (flip tryPutMVar ())
  putMVar (lockConditions lock) conds

-- relinquish ownership, release lock, block until signal, reacquire lock, reinstate ownership
-- should only be called by current owner
awaitImpl :: ReentrantLock -> MVar () -> IO ()
awaitImpl l newCond = do
  tid <- myThreadId
  mask_ $ do
    currentOwner <- swapMVar (lockOwner l) NoOwner
    putMVar (innerLock l) ()
    takeMVar newCond
    let retryUntilNoOwner = do
          owner <- takeMVar (lockOwner l)
          case owner of
            NoOwner -> do
              takeMVar (innerLock l)
              putMVar (lockOwner l) currentOwner
            Owner oTid c -> if tid == oTid
                              then takeMVar (innerLock l) >> putMVar (lockOwner l) (Owner oTid (succ c))
                              else putMVar (lockOwner l) owner >> readMVar (innerLock l) >> retryUntilNoOwner
    retryUntilNoOwner

registerCondition :: ReentrantLock -> IO (Condition ReentrantLock)
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
  return $ Condition (awaitImpl l newCond) (signalImpl l) (signalAllImpl l) destroyImpl


